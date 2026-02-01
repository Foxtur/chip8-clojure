(ns chip8.web
  (:require [org.httpkit.server :as server]
            [hiccup2.core :as h]
            [chip8.core :as cpu]))

(defn home-page []
  (str
   (h/html
    [:html
     [:head
      [:title "Datastar Chip-8"]
      ;; Using the exact v1.0 RC bundle
      [:script {:type "module"
                :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"}]]
     [:body
      [:h1 "Clojure Chip-8 + Datastar"]
      ;; data-init is the new way to trigger an action on load
      [:div {:data-init "@get('/stream')"}
       [:div#display "Connecting..."]]]])))

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
    (server/send! channel {:headers {"Content-Type" "text/event-stream"
                                     "Cache-Control" "no-cache"}} false)
    (let [test-cpu (-> (cpu/init-cpu) (cpu/load-rom "roms/IBM Logo.ch8"))]
      (future
        (loop [curr-cpu test-cpu]
          (when (server/open? channel)
            ;; step the emulator
            (let [next-cpu (cpu/step curr-cpu)]
              (send-fragment! channel (render-display next-cpu))
              (Thread/sleep 16)
              (recur next-cpu))))))))

(defn app [{:keys [uri] :as req}]
  (case uri
    "/" {:status 200 :body (home-page)}
    "/stream" (stream-handler req)
    {:status 404 :body "Not Found"}))

(defn -main [& args]
  (server/run-server app {:port 8080})
  (println "Server started on http://localhost:8080"))
