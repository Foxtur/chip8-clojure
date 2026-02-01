(ns chip8.web
  (:require [org.httpkit.server :as server]
            [ring.middleware.params :refer [wrap-params]]
            [hiccup2.core :as h]
            [chip8.core :as cpu]))

(def global-cpu (atom (-> (cpu/init-cpu)
                          (cpu/load-rom "roms/pong.ch8"))))

;; This runs once when the server starts
(defn start-emulator-thread! []
  (future
    (loop []
      (when-not (:paused? @global-cpu)
        (swap! global-cpu (fn [cpu]
                            (let [cpu-after-inst (nth (iterate cpu/step cpu) 10)]
                              (cpu/decrement-timers cpu-after-inst)))))
      (Thread/sleep 16) ;; Aim for 60Hz
      (recur))))

(defn home-page []
  (str
   (h/html
    [:html
     [:head
      [:title "Datastar Chip-8"]
      [:script {:type "module" 
                :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"}]]
     (h/raw (str "<body data-signals=\"{}\" "
                 "      data-on:keydown__window=\"@get('/input?key=' + evt.key)\" "
                 "      data-on:keyup__window=\"@get('/input?key=' + evt.key + '&type=up')\">"))
     [:h1 "Datastar Chip-8"]
     (h/raw "<div data-init=\"@get('/stream')\">")
       [:div#display "Connecting..."]
     (h/raw "</div>")
     (h/raw "</body>")])))

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

(defn stream-handler [req]
  (server/with-channel req channel
    ;; Send initial headers to keep connection open
    (server/send! channel {:headers {"Content-Type" "text/event-stream"
                                     "Cache-Control" "no-cache"
                                     "Connection" "keep-alive"}} false)

    (println "Client connected to stream.")

    (future
      (loop [last-display nil]
        (if (server/open? channel)
          (let [current-state @global-cpu
                current-display (:display current-state)]

            ;; OPTIMIZATION: Only send a fragment if the display has actually changed
            (when (not= last-display current-display)
              (send-fragment! channel (render-display current-state)))

            (Thread/sleep 16) ;; Sync with 60Hz
            (recur current-display))
          (println "Client disconnected."))))))

(defn input-handler [{:keys [params] :as req}]
  (println "Input received:" params) ;; <--- ADD THIS
  (let [key-str (get params "key")
        key-char (when (seq key-str) (first key-str))
        type (get params "type")
        hex (get cpu/key-map key-char)]
    (when hex
      (if (= type "up")
        (swap! global-cpu update :keypad disj hex)
        (swap! global-cpu update :keypad conj hex)))
    {:status 204})) ;; 204 No Content tells the browser 'Done, no UI change needed'

(def app (wrap-params
          (fn [{:keys [uri params] :as req}]
            (case uri
              "/" {:status 200 :headers {"Content-Type" "text/html"} :body (home-page)}
              "/stream" (stream-handler req)
              "/input"  (input-handler req)
              {:status 404 :body "Not Found"}))))

(defn -main [& args]
  (start-emulator-thread!)
  (server/run-server app {:port 8080})
  (println "Server started on http://localhost:8080"))
