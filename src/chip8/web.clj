(ns chip8.web
  (:require [org.httpkit.server :as server]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]]
            [hiccup2.core :as h]
            [chip8.core :as cpu]))

(def active-games (atom {}))

(defn home-page []
  (let [sid (str (java.util.UUID/randomUUID))]
    (str
     (h/html
      [:html
       [:head
        [:title "Isolated Chip-8"]
        [:script {:type "module"
                  :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"}]
        [:script (h/raw "window.addEventListener('keydown', (e) => console.log('Physical Key:', e.key));")]]

       [:body
        [:h1 "Session: " sid]
        (h/raw (str "<div "
                    ;; Manually build the URL string to force Query Parameters
                    "     data-on:keydown__window=\"@get('/input?sid=" sid "&key=' + evt.key)\" "
                    "     data-on:keyup__window=\"@get('/input?sid=" sid "&key=' + evt.key + '&type=up')\">"
                    "  <div data-init=\"@get('/stream?sid=" sid "')\">"
                    "    <div id='display'>Connecting...</div>"
                    "  </div>"
                    "</div>"))
        (h/raw "</body>")]]))))

;; Wasteful Div-Grid approach
#_(defn home-page []
    (str
     (h/html
      [:html
       [:head
        [:title "Datastar Div-Grid Chip-8"]
        [:script {:type "module"
                  :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-RC.7/bundles/datastar.js"}]
        [:style "
        #display {
          display: grid;
          grid-template-columns: repeat(64, 10px);
          grid-template-rows: repeat(32, 10px);
          background: #222;
          width: 640px;
        }
        .pixel { width: 10px; height: 10px; }
        .on { background: white; }
        .off { background: black; }
      "]]
       [:body
        [:h1 "Div-Grid Rendering"]
        [:div {:data-init "@get('/stream')"}
         [:div#display "Connecting..."]]]])))

(defn render-display [cpu]
  (let [display (:display cpu)
        width 64
        height 32
        scale 10]
    (str
     (h/html
      [:svg#display {:width (* width scale)
                     :height (* height scale)
                     :viewbox (str "0 0 " width " " height)
                     :style "background: black; shape-rendering: crispEdges;"}
          ;; Iterate through display buffer
       (for [idx (range (count display))
             :let [pixel (nth display idx)]
             :when (= pixel 1)
             :let [x (mod idx width)
                   y (quot idx width)]]
         [:rect {:x x :y y :width 1 :height 1 :fill "white"}])]))))

(defn render-display-divs [cpu]
  (let [display (:display cpu)]
    (str
     (h/html
      [:div#display
       (for [pixel display]
         [:div {:class (str "pixel " (if (= pixel 1) "on" "off"))}])]))))

(defn send-fragment! [channel html-str]
  (server/send! channel
                (str "event: datastar-patch-elements\n"
                     "data: elements " html-str "\n\n")
                false))

(defn stream-handler [{:keys [params] :as req}]
  (let [sid (get params "sid")]
    (println "Stream connected for SID:" sid)
    (server/with-channel req channel
      (server/send! channel {:headers {"Content-Type" "text/event-stream"}} false)
      (let [user-cpu (atom (-> (cpu/init-cpu) (cpu/load-rom "resources/roms/Pong.ch8")))]
        (swap! active-games assoc sid user-cpu)
        (future
          (loop [last-display nil]
            (if (server/open? channel)
              (let [next-cpu (-> (nth (iterate cpu/step @user-cpu) 10) cpu/decrement-timers)]
                (reset! user-cpu next-cpu)
                (when (not= last-display (:display next-cpu))
                  (send-fragment! channel (render-display next-cpu)))
                (Thread/sleep 16)
                (recur (:display next-cpu)))
              (swap! active-games dissoc sid))))))))

(defn input-handler [{:keys [params query-string] :as req}]
  (let [sid (get params "sid")
        key-val (get params "key")
        user-atom (get @active-games sid)]
    (println "DEBUG: SID=" sid " | Key=" key-val " | Found=" (some? user-atom))
    (println "Full Req Params:" (:params req))
    (when user-atom
      (let [key-str (get params "key")
            key-char (when (= 1 (count key-str)) (first key-str))
            hex (get cpu/key-map key-char)]
        (when hex
          (if (= (get params "type") "up")
            (swap! user-atom update :keypad disj hex)
            (swap! user-atom update :keypad conj hex)))))
    {:status 204}))

(defn app-handler [{:keys [uri params] :as req}]
  (case uri
    "/"       {:status 200 :headers {"Content-Type" "text/html"} :body (home-page)}
    "/stream" (stream-handler req)
    "/input"  (input-handler req)
    {:status 404 :body "Not Found"}))

(def app (wrap-params app-handler))

(defn -main [& args]
  (server/run-server app {:port 8080})
  (println "Server started on http://localhost:8080"))
