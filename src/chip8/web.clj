(ns chip8.web
  (:require [org.httpkit.server :as server]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [hiccup2.core :as h]
            [chip8.core :as cpu]
            [clojure.java.io :as io]))

(def active-games (atom {}))

(defn list-roms []
  (let [dir (io/file "resources/roms")]
    (if (.exists dir)
      (sort (map #(.getName %) (.listFiles dir)))
      ["IBM Logo.ch8"]))) ;; Fallback

(defn home-page []
  (let [sid (str (java.util.UUID/randomUUID))
        roms (list-roms)]
    (str
     (h/html
      [:html
       [:head
        [:title "Isolated Chip-8"]
        [:script {:type "module"
                  :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"}]
        [:script (h/raw "window.addEventListener('keydown', (e) => console.log('Physical Key:', e.key));")]

        [:style (h/raw "
        body { 
          background: #050505; 
          color: #00FF41; 
          font-family: 'Courier New', monospace; 
          display: flex; flex-direction: column; align-items: center;
        }
        
        /* The CRT Screen Container */
        #display-container {
          position: relative;
          padding: 20px;
          background: #111;
          border: 10px solid #333;
          border-radius: 20px;
          box-shadow: 0 0 50px rgba(0, 255, 65, 0.2);
          overflow: hidden;
        }

        /* The Phosphor Glow Effect */
        #display {
          background: #000;
          shape-rendering: crispEdges;
          filter: drop-shadow(0 0 2px rgba(0, 255, 65, 0.8));
        }

        /* Scanlines Overlay */
        #display-container::after {
          content: ' ';
          display: block;
          position: absolute;
          top: 0; left: 0; bottom: 0; right: 0;
          background: linear-gradient(rgba(18, 16, 16, 0) 50%, rgba(0, 0, 0, 0.25) 50%), 
                      linear-gradient(90deg, rgba(255, 0, 0, 0.06), rgba(0, 255, 0, 0.02), rgba(0, 0, 255, 0.06));
          background-size: 100% 4px, 3px 100%;
          pointer-events: none; /* Let clicks pass through */
          z-index: 10;
        }

        /* Slight flickering for realism */
        @keyframes flicker {
          0% { opacity: 0.98; }
          50% { opacity: 1; }
          100% { opacity: 0.99; }
        }
        #display { animation: flicker 0.1s infinite; }
      ")]]

       [:body
        [:h1 "Session: " sid]

        [:div {:style "margin-bottom: 20px;"}
         [:label "Choose ROM: "]
         (h/raw (str "<select "
                     "  data-on:change=\"@get('/load?sid=" sid "&rom=' + evt.target.value)\">"
                     (apply str (for [r roms] (str "<option value='" r "'>" r "</option>")))
                     "</select>"))]

        (h/raw (str "<div "
                    ;; Manually build the URL string to force Query Parameters
                    "     data-on:keydown__window=\"@get('/input?sid=" sid "&key=' + evt.key)\" "
                    "     data-on:keyup__window=\"@get('/input?sid=" sid "&key=' + evt.key + '&type=up')\">"
                    "  <div data-init=\"@get('/stream?sid=" sid "')\">"
                    "    <div id='display-container'>"
                    "      Connecting..."
                    "    </div>"
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
        scale 10
        sound-active? (and (> (:sound cpu) 0) (< (:sound cpu) 5))]
    (str
     (h/html
      [:div#display-container
          ;; the screen
       [:svg#display {:width (* width scale)
                      :height (* height scale)
                      :viewbox (str "0 0 " width " " height)
                      :style "background: black; shape-rendering: crispEdges;"}
        (for [idx (range (count display))
              :let [pixel (nth display idx)]
              :when (= pixel 1)
              :let [x (mod idx width)
                    y (quot idx width)]]
          [:rect {:x x :y y :width 1 :height 1 :fill "#00FF41"}])]
          ;; sound trigger if timer is > 0
       (when sound-active?
         [:audio {:src "/soundeffect.mp3"
                  :autoplay true}])]))))

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
      (let [user-cpu (atom (-> (cpu/init-cpu) (cpu/load-rom "resources/roms/IBM Logo.ch8")))]
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

(defn input-handler [{:keys [params] :as req}]
  (let [sid (get params "sid")
        user-atom (get @active-games sid)]
    (when user-atom
      (let [key-str (get params "key")
            key-char (when (= 1 (count key-str)) (first key-str))
            hex (get cpu/key-map key-char)]
        (when hex
          (if (= (get params "type") "up")
            (swap! user-atom update :keypad disj hex)
            (swap! user-atom update :keypad conj hex)))))
    {:status 204}))

(defn load-handler [{:keys [params]}]
  (let [sid (get params "sid")
        rom-name (get params "rom")
        user-atom (get @active-games sid)]
    (when (and user-atom rom-name)
      (println "Resetting session" sid "to ROM:" rom-name)
      ;; 1. Initialize a fresh CPU
      ;; 2. Load the new ROM
      ;; 3. reset! the existing atom so the stream loop picks it up instantly
      (reset! user-atom (-> (cpu/init-cpu)
                            (cpu/load-rom (str "resources/roms/" rom-name)))))
    {:status 204 :body ""}))


(defn app-handler [{:keys [uri] :as req}]
  (case uri
    "/"       {:status 200 :headers {"Content-Type" "text/html"} :body (home-page)}
    "/stream" (stream-handler req)
    "/input"  (input-handler req)
    "/load"   (load-handler req)
    {:status 404 :body "Not Found"}))

(def app
  (-> app-handler
      wrap-params
      (wrap-resource "public")))

(defn -main [& args]
  (server/run-server app {:port 8080})
  (println "Server started on http://localhost:8080"))
