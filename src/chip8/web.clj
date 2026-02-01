(ns chip8.web
  (:require [org.httpkit.server :as server]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [hiccup2.core :as h]
            [charred.api :as json]
            [camel-snake-kebab.core :as csk]
            [chip8.core :as cpu]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def active-games (atom {}))

(defn start-game-loop! [sid]
  (let [cpu-atom (atom (-> (cpu/init-cpu) (cpu/load-rom "resources/roms/IBM Logo.ch8")))]
    (future
      (loop []
        (if (get @active-games sid) ;; only run if the session is still active
          (do
            (when-not (:paused? @cpu-atom)
              (swap! cpu-atom (fn [cpu] (-> (nth (iterate cpu/step cpu) 10) cpu/decrement-timers))))
            (Thread/sleep 16)
            (recur))
          (println "Stopping loop for" sid))))
    cpu-atom))

(defn list-roms []
  (let [dir (io/file "resources/roms")]
    (if (.exists dir)
      (sort (map #(.getName %) (.listFiles dir)))
      ["IBM Logo.ch8"]))) ;; Fallback

(defn home-page []
  (let [sid (str (java.util.UUID/randomUUID))
        roms (list-roms)
        ;; Generate the <option> strings manually
        rom-options (apply str (for [r roms] (str "<option value='" r "'>" r "</option>")))
        ;; Read the template file from resources
        template (slurp (io/resource "public/index.html"))]
    ;; Perform string replacement
    (-> template
        (str/replace "{{sid}}" sid)
        (str/replace "{{rom_options}}" rom-options)
        (str/replace "{{initial_mode}}" "optimized"))))

(defn render-display-naive-divs [display sound? paused?]
  (str (h/html
        [:div#display-container
         [:div#display {:style "display: grid; grid-template-columns: repeat(64, 10px); background: #000; width: 640px;"}
          (for [p display]
            [:div {:style (str "width: 10px; height: 10px; background: " (if (= p 1) "#00FF41" "transparent") ";")}])]
         (when (and sound? (not paused?)) [:audio {:src "/soundeffect.mp3" :autoplay true}])])))

(defn render-display-full-svg [display sound-active? paused?]
  (let [width 64 height 32 scale 10]
    (str
     (h/html
      [:div#display-container
       [:svg#display {:width (* width scale) :height (* height scale) :viewBox "0 0 64 32" :style "background: black;"}
        ;; Iterate and draw EVERY single pixel as a rect
        (for [idx (range (count display))
              :let [pixel (nth display idx)
                    x (mod idx width)
                    y (quot idx width)]]
          [:rect {:x x :y y :width 1 :height 1
                  :fill (if (= pixel 1) "#00FF41" "#111")}])] ;; Dark gray for 'off'

       (when paused? [:text {:x 32 :y 16 :fill "red" :font-size "5" :text-anchor "middle"} "PAUSED"])
       (when (and sound-active? (not paused?))
         [:audio {:id (str "b-" (System/currentTimeMillis)) :src "/soundeffect.mp3" :autoplay true}])]))))

(defn render-display-granular [display sound-active? paused?]
  (let [width 64 height 32 scale 10]
    (str
     (h/html
      ;; Target the display-container, but use "morph" merge
      ;; to ensure Datastar only touches what changed.
      [:div#display-container {:data-merge "morph"}
       [:svg#display {:width (* width scale) :height (* height scale) :viewBox "0 0 64 32"
                      :style "background: black; shape-rendering: crispEdges;"}

        ;; Only send the pixels that are ON.
        ;; Morphing will remove the ones that disappear.
        (for [idx (range (count display))
              :let [pixel (nth display idx)]
              :when (= pixel 1)]
          [:rect {:id (str "p-" idx) ;; ID is key for granular morphing performance
                  :x (mod idx width) :y (quot idx width)
                  :width 1 :height 1 :fill "#00FF41"}])

        ;; Draw Pause Text
        (when paused?
          [:text {:id "pause-label" :x 32 :y 16 :fill "red" :font-size "5"
                  :text-anchor "middle" :font-family "monospace"} "PAUSED"])]

       ;; Sound Trigger: ID must be unique to force fresh play
       (when (and sound-active? (not paused?))
         [:audio {:id (str "beep-" (System/currentTimeMillis))
                  :src "/soundeffect.mp3" :autoplay true}])]))))

(defn send-fragment! [channel html-str]
  (server/send! channel
                (str "event: datastar-patch-elements\n"
                     "data: elements " html-str "\n\n")
                false))

(defn datastar-params->json [params]
  (json/read-json params :key-fn csk/->kebab-case-keyword))

(defn stream-handler [{:keys [params] :as req}]
  (let [sid (get params "sid")
        datastar (datastar-params->json (get params "datastar"))
        mode (get datastar :render-mode "optimized")]
    ;; Get the existing game for this session, or start a new one if it's the first time
    (let [user-cpu (or (get-in @active-games [sid :cpu-atom])
                       (let [new-atom (start-game-loop! sid)]
                         (swap! active-games assoc-in [sid :cpu-atom] new-atom)
                         new-atom))]
      
      (server/with-channel req channel
        (server/send! channel {:headers {"Content-Type" "text/event-stream"}} false)
        
        ;; This future ONLY handles sending data to THIS specific connection
        (future
          (loop [last-display nil]
            (if (server/open? channel)
              (let [cpu-state @user-cpu]
                (when (not= last-display (:display cpu-state))
                  (let [html (case mode
                               "full-frame" (render-display-full-svg (:display cpu-state) (> (:sound cpu-state) 0) (:paused? cpu-state))
                               "naive-divs" (render-display-naive-divs (:display cpu-state) (> (:sound cpu-state) 0) (:paused? cpu-state))
                               (render-display-granular (:display cpu-state) (> (:sound cpu-state) 0) (:paused? cpu-state)))]
                    (send-fragment! channel html)))
                (Thread/sleep 16)
                (recur (:display cpu-state)))
              (println "Stream connection closed for" sid))))))))

(defn input-handler [{:keys [params]}]
  (let [sid (get params "sid")
        user-atom (get-in @active-games [sid :cpu-atom])]
    (when user-atom
      (let [key-val (get params "key")
            type (get params "type")]
        (cond
          ;; Toggle pause on 'p' keydown
          (and (= key-val "p") (not= type "up"))
          (swap! user-atom update :paused? not)

          ;; Handle regular keypad keys
          :else
          (let [key-char (when (= 1 (count key-val)) (first key-val))
                hex (get cpu/key-map key-char)]
            (when hex
              (if (= type "up")
                (swap! user-atom update :keypad disj hex)
                (swap! user-atom update :keypad conj hex)))))))
    {:status 204 :body ""}))

(defn load-handler [{:keys [params]}]
  (let [sid (get params "sid")
        datastar (datastar-params->json (get params "datastar"))
        rom-name (get datastar :rom "IBM Logo.ch8")
        user-atom (get-in @active-games [sid :cpu-atom])]
    (when (and user-atom rom-name)
      (println "Resetting session" sid "to ROM:" rom-name)
      ;; reset! the existing atom so the stream loop picks it up instantly
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
