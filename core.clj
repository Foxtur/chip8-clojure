(ns chip8.core)

;; The Chip-8 has
;; 4096 bytes of memory
;; 16 working registers (V0 through VF)
;; A 16-bit Index register (I)
;; A Program Counter (PC)
;; Two timers (delay and sound)
;; A stack for subroutines
;; A display buffer (64x32 pixels)

(defn init-cpu []
  {:memory (byte-array 4096)
   :v (vec (repeat 16 0)) ;; 16 registers initialized to 0
   :i 0 ;; Index register
   :pc 0x200 ;; Programs start at 0x200
   :stack []
   :delay 0
   :sound 0
   :display (vec (repeat (* 64 32) 0)) ;; Flat vector for the screen
   :keypad (set nil)}) ;; currently pressed keys

(defn -main []
  (let [cpu (init-cpu)]
    (println "Chip-8 Initialized.")
    (println "PC starts at:" (format "0x%X" (:pc cpu)))
    (println "Registers:" (:v cpu))))
