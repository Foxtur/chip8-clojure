(ns chip8.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m])
  (:import [javax.swing JFileChooser]
           [javax.swing.filechooser FileNameExtensionFilter]))

(defn- copy-vec-to-array!
  [src-vec dest offset]
  (let [src-arr (byte-array src-vec)] ; Convert vector to temp array
    (System/arraycopy src-arr 0 dest offset (count src-vec))))

(defn- copy-to-array!
  [src dest offset]
  (System/arraycopy src 0 dest offset (count src)))

(def font-set
  [0xF0 0x90 0x90 0x90 0xF0 ;; 0
   0x20 0x60 0x20 0x20 0x70 ;; 1
   0xF0 0x10 0xF0 0x80 0xF0 ;; 2
   0xF0 0x10 0xF0 0x10 0xF0 ;; 3
   0x90 0x90 0xF0 0x10 0x10 ;; 4
   0xF0 0x80 0xF0 0x10 0xF0 ;; 5
   0xF0 0x80 0xF0 0x90 0xF0 ;; 6
   0xF0 0x10 0x20 0x40 0x40 ;; 7
   0xF0 0x90 0xF0 0x90 0xF0 ;; 8
   0xF0 0x90 0xF0 0x10 0xF0 ;; 9
   0xF0 0x90 0xF0 0x90 0x90 ;; A
   0xE0 0x90 0xE0 0x90 0xE0 ;; B
   0xF0 0x80 0x80 0x80 0xF0 ;; C
   0xE0 0x90 0x90 0x90 0xE0 ;; D
   0xF0 0x80 0xF0 0x80 0xF0 ;; E
   0xF0 0x80 0xF0 0x80 0x80 ;; F
   ])

(defn load-font
  "Takes a `cpu` and loads the `font-set`
   into memory at location 0x00"
  [cpu]
  (let [memory (:memory cpu)]
    (copy-vec-to-array! font-set memory 0)
    cpu))

;; The Chip-8 has
;; 4096 bytes of memory
;; 16 working registers (V0 through VF)
;; A 16-bit Index register (I)
;; A Program Counter (PC)
;; Two timers (delay and sound)
;; A stack for subroutines
;; A display buffer (64x32 pixels)

(defn init-cpu []
  (-> {:memory (byte-array 4096)
       :v (vec (repeat 16 0)) ;; 16 registers initialized to 0
       :i 0 ;; Index register
       :pc 0x200 ;; Programs start at 0x200
       :stack []
       :delay 0
       :sound 0
       :display (vec (repeat (* 64 32) 0)) ;; Flat vector for the screen
       :keypad (set nil) ;; currently pressed keys
       :paused? false} 
      (load-font)))

(defn wrap-byte [n]
  (bit-and n 0xFF))

(defn read-mem
  "Reads memory `addr` from `cpu`"
  [cpu addr]
  (wrap-byte (aget ^bytes (:memory cpu) addr)))

(defn write-mem [cpu addr val]
  {:pre [(<= val 255)]}
  (aset ^bytes (:memory cpu) addr (unchecked-byte val))
  cpu) ;; return CPU so we can chain operations

(defn read-reg [cpu reg-idx]
  (get-in cpu [:v reg-idx]))

(defn write-reg
  "Writes `val` to register `reg-idx` of `cpu`

   Returns: the updated cpu"
  [cpu reg-idx val]
  (assoc-in cpu [:v reg-idx] (wrap-byte val)))

(defn write-pixel
  [cpu x y new-val]
  ;; pixel(x,y) = y * 64 + x
  (let [idx (+ (* (mod y 32) 64) (mod x 64))
        val (nth (:display cpu) idx)
        xor-val (bit-xor val new-val)
        collision (if (and (= val 1) (= xor-val 0)) 1 0)]
    (-> cpu
        (assoc-in [:display idx] xor-val)
        (write-reg 0xF collision))))

(defn write-sprite [cpu x y sprite-addr]
  (let [data (read-mem cpu sprite-addr)]
    (loop [cpu cpu
           idx 7
           collision 0]
      (let [val (if (bit-test data idx) 1 0)]
        (if (>= idx 0)
          (let [cpu (write-pixel cpu (+ x (- 7 idx)) y val)
                dirty (read-reg cpu 0xF)
                cpu (if (= 1 collision) (write-reg cpu 0xF 1) cpu)]
            (recur cpu (dec idx) (if (= 1 collision) 1 dirty)))
          cpu)))))

(defn load-font [cpu]
  (reduce-kv (fn [c idx val]
               (write-mem c idx val))
             cpu
             font-set))
(defn load-rom [cpu path-or-res]
  (let [source (let [f (io/file path-or-res)]
                 (if (.exists f) f (io/resource path-or-res)))]
    (if source
      (with-open [is (io/input-stream source)]
        (let [rom-bytes (.readAllBytes is)
              rom-vec (vec rom-bytes)]
          (reduce-kv (fn [c idx val]
                       (write-mem c (+ 0x200 idx) val))
                     cpu
                     rom-vec)))
      (do
        (println "Error: Could not find ROM at" path-or-res)
        cpu))))

(defn decode-opcode [opcode]
  {:op (bit-and opcode 0xF000) ;; First nibble
   :x  (bit-shift-right (bit-and opcode 0x0F00) 8) ;; Second nibble
   :y  (bit-shift-right (bit-and opcode 0x00F0) 4) ;; Third nibble
   :n  (bit-and opcode 0x000F) ;; Last nibble
   :nn (bit-and opcode 0x00FF) ;; Last byte
   :nnn (bit-and opcode 0x0FFF)}) ;; Last 12 byte

(defn increment-pc [cpu]
  (update cpu :pc + 2))

(defn decrement-timers [cpu]
  (assoc cpu
         :delay (max 0 (dec (:delay cpu)))
         :sound (max 0 (dec (:sound cpu)))))

(defn jump-pc [cpu addr]
  (assoc cpu :pc addr))

(defn push-stack [cpu addr]
  ;; We use a vector as a stack; conj adds to the end
  (update cpu :stack conj addr))

(defn pop-stack
  "Takes a `cpu` and returns a pair `[new-cpu-state popped-address]`"
  [cpu]
  (let [addr (last (:stack cpu))
        new-cpu (update cpu :stack pop)]
    [new-cpu addr]))

(defn fetch-opcode [cpu]
  (let [pc (:pc cpu)
        byte1 (read-mem cpu pc)
        byte2 (read-mem cpu (inc pc))]
    ;; Merge two 8-bit bytes into one 16bit opcode
    (bit-or (bit-shift-left byte1 8) byte2)))

(defn step [cpu]
  (let [opcode (fetch-opcode cpu)
        {:keys [op x y n nn nnn] :as decoded} (decode-opcode opcode)
        ;; _ (println (format "PC: 0x%X | OP: 0x%X" (:pc cpu) op))
        ;; Default: move to next instruction
        cpu-stepped (increment-pc cpu)
        pc (:pc cpu)]
    (when (> pc 4090) (println "CRASH IMMINENT: PC is" pc))
    (case op
      0x0000 (case nn
               ;; CLS
               0xE0 (assoc cpu-stepped :display (vec (repeat 2048 0)))
               ;; RET
               0xEE (let [[popped-cpu addr] (pop-stack cpu-stepped)]
                      (assoc popped-cpu :pc addr))
               cpu-stepped)
      ;; JMP
      0x1000 (assoc cpu-stepped :pc nnn)
      ;; CALL addr
      0x2000 (-> cpu-stepped
                 (push-stack (:pc cpu-stepped))
                 (assoc :pc nnn))
      ;; SE Vx, byte
      0x3000 (let [vx (read-reg cpu-stepped x)]
               (if (= vx nn)
                 (increment-pc cpu-stepped)
                 cpu-stepped))
      ;; SNE Vx, byte
      0x4000 (let [vx (read-reg cpu-stepped x)]
               (if (not= vx nn)
                 (increment-pc cpu-stepped)
                 cpu-stepped))
      ;; SE Vx, Vy
      0x5000 (let [vx (read-reg cpu-stepped x)
                   vy (read-reg cpu-stepped y)]
               (if (not= n 0)
                 (throw (IllegalStateException. "5XY0 - Trailing nibble must end in 0")))
               (if (= vx vy)
                 (increment-pc cpu-stepped)
                 cpu-stepped))
      ;; LD Vx, byte
      0x6000 (write-reg cpu-stepped x nn)
      ;; ADD Vx, byte
      0x7000 (let [old-val (read-reg cpu-stepped x)]
               (write-reg cpu-stepped x (+ old-val nn)))
      0x8000 (let [vx (read-reg cpu-stepped x)
                   vy (read-reg cpu-stepped y)]
               (case n
                 ;; LD Vx, Vy
                 0x0 (write-reg cpu-stepped x vy)
                 ;; OR Vx, Vy
                 0x1 (write-reg cpu-stepped x (bit-or vx vy))
                 ;; AND Vx, Vy
                 0x2 (write-reg cpu-stepped x (bit-and vx vy))
                 ;; XOR Vx, Vy
                 0x3 (write-reg cpu-stepped x (bit-xor vx vy))
                 ;; ADD Vx, Vy
                 0x4 (let [result (+ vx vy)
                           flag (if (> result 255) 1 0)]
                       (-> cpu-stepped
                           (write-reg x result)
                           (write-reg 0xF flag)))
                 ;; SUB Vx, Vy
                 0x5 (let [result (- vx vy)
                           flag (if (> vx vy) 1 0)]
                       (-> cpu-stepped
                           (write-reg x result)
                           (write-reg 0x0F flag)))
                 ;; SHR Vx{, Vy}
                 0x6 (let [lsb (unsigned-bit-shift-right (bit-and 0x0F vx) 3)
                           shifted (bit-shift-right vx 1)]
                       (-> cpu-stepped
                           (write-reg x shifted)
                           (write-reg 0x0F lsb)))
                 ;; SUBN Vx, Vy
                 0x7 (let [flag (if (> vy vx) 1 0)
                           result (- vy vx)]
                       (-> cpu-stepped
                           (write-reg x result)
                           (write-reg 0x0F flag)))
                 ;; SHL Vx{, Vy}
                 0xE (let [msb (unsigned-bit-shift-right (bit-and 0xF0 vx) 7)
                           shifted (bit-shift-left vx 1)]
                       (-> cpu-stepped
                           (write-reg x shifted)
                           (write-reg 0x0F msb)))
                 cpu-stepped))
      ;; SNE Vx, Vy
      0x9000 (let [vx (read-reg cpu-stepped x)
                   vy (read-reg cpu-stepped y)]
               (if (not= vx vy)
                 (increment-pc cpu-stepped)
                 cpu-stepped))
      ;; LD I, addr
      0xA000 (assoc cpu-stepped :i nnn)
      ;; JP V0, addr
      0xB000 (let [v0 (read-reg cpu-stepped 0)]
               (assoc cpu-stepped :pc (+ nnn v0)))
      ;; RND Vx, byte
      0xC000 (let [rand-byte (rand-int 256)]
               (write-reg cpu-stepped x (bit-and rand-byte nn)))
      ;; DRW Vx, Vy, nibble
      0xD000 (let [vx (read-reg cpu-stepped x)
                   vy (read-reg cpu-stepped y)]
               #_(println (format "DRAW at (%d, %d) height %d. I is 0x%X"
                                vx vy n (:i cpu-stepped)))
               (reduce (fn [cpu row]
                         (write-sprite cpu vx (+ vy row) (+ (:i cpu) row)))
                       cpu-stepped
                       (range n)))
      0xE000 (let [vx (read-reg cpu-stepped x)
                   pressed? (contains? (:keypad cpu-stepped) vx)]
               (case nn
                 ;; SKP Vx
                 0x9E (if pressed?
                        (increment-pc cpu-stepped)
                        cpu-stepped)
                 ;; SKPNP Vx
                 0xA1 (if pressed?
                        cpu-stepped
                        (increment-pc cpu-stepped))
                 cpu-stepped))
      0xF000 (case nn
               ;; LD Vx, DT
               0x07 (write-reg cpu-stepped x (:delay cpu-stepped))
               ;; LD DT, Vx
               0x15 (let [vx (read-reg cpu-stepped x)]
                      (assoc cpu-stepped :delay vx))
               ;; LD ST, Vx
               0x18 (let [vx (read-reg cpu-stepped x)]
                      (assoc cpu-stepped :sound vx))
               ;; ADD I, Vx
               0x1E (let [vx (read-reg cpu-stepped x)]
                      (update cpu-stepped :i + vx))
               ;; LD F, Vx
               0x29 (let [vx (read-reg cpu-stepped x)
                          addr (* 5 vx)]
                      (assoc cpu-stepped :i addr))
               ;; LD B, Vx
               0x33 (let [vx (read-reg cpu-stepped x)
                          addr (:i cpu-stepped)
                          hundreds (quot vx 100)
                          tens (quot (rem vx 100) 10)
                          ones (rem vx 10)]
                      (-> cpu-stepped
                          (write-mem (+ addr 0) hundreds)
                          (write-mem (+ addr 1) tens)
                          (write-mem (+ addr 2) ones)))
               ;; LD[I], Vx
               0x55 (reduce
                     (fn [cpu idx]
                       (let [addr (:i cpu)
                             val (read-reg cpu idx)]
                         (write-mem cpu (+ addr idx) val)))
                     cpu-stepped
                     (range (inc x)))
               ;; LD Vx, [I]
               0x65 (reduce
                     (fn [cpu idx]
                       (let [addr (:i cpu)
                             val (read-mem cpu (+ addr idx))]
                         (write-reg cpu idx val)))
                     cpu-stepped
                     (range (inc x)))
               ;; LD Vx, K
               0x0A (let [vx (read-reg cpu-stepped x)]
                      (if (empty? (:keypad cpu-stepped))
                        cpu
                        (write-reg cpu-stepped x (first (:keypad cpu-stepped)))))
               cpu-stepped)
      ;; Default case for unimplemented opcodes
      cpu-stepped)))

;;; quil

(defn choose-and-load-rom [cpu]
  (let [chooser (JFileChooser. "resources/roms/")
        _ (.setFileFilter chooser (FileNameExtensionFilter. "Chip-8 ROMs" (into-array ["ch8"])))
        return-val (.showOpenDialog chooser nil)]
    (if (= return-val JFileChooser/APPROVE_OPTION)
      (let [file (.getSelectedFile chooser)
            path (.getAbsolutePath file)
            ;; Create a totally fresh CPU and load the ROM into it
            new-cpu (load-rom (init-cpu) path)]
        (if (nil? new-cpu)
          (do (println "WARNING: load-rom returned nil! Keeping old state.") cpu)
          (do (println "Switching to new ROM...") new-cpu)))
      ;; If user clicks 'Cancel' or closes the window
      (do (println "Load cancelled.") cpu))))

;; 1 2 3 C      (Physical: 1 2 3 4)
;; 4 5 6 D      (Physical: Q W E R)
;; 7 8 9 E      (Physical: A S D F)
;; A 0 B F      (Physical: Z X C V)
(def key-map
  {\1 0x1, \2 0x2, \3 0x3, \4 0xC,
   \q 0x4, \w 0x5, \e 0x6, \r 0xD,
   \a 0x7, \s 0x8, \d 0x9, \f 0xE,
   \z 0xA, \x 0x0, \c 0xB, \v 0xF})

(defn on-key-pressed [cpu event]
  (let [key (:raw-key event)
        hex (get key-map key)]
    (cond
      (= key \o) (choose-and-load-rom cpu)
      (= key \p) (update cpu :paused? not)
      (some? hex) (update cpu :keypad conj hex)
      :else cpu)))

(defn on-key-released [cpu event]
  (if-let [hex (get key-map (:raw-key event))]
    (update cpu :keypad disj hex)
    cpu))

(defn setup [rom-file]
  (q/frame-rate 120)
  ;; initial state
  (-> (init-cpu)
      (load-rom rom-file)))

(defn update-state [cpu]
  (if (nil? cpu) 
    nil
    (if (:paused? cpu)
      cpu
      (let [sound-val (get cpu :sound 0)
            cpu-after-inst (nth (iterate step cpu) 10)]
        (when (> sound-val 0)
          (.beep (java.awt.Toolkit/getDefaultToolkit)))
        (decrement-timers cpu-after-inst)))))

(defn draw-state [cpu]
  (q/background 0)
  (q/no-stroke)
  (q/fill 255)
  (let [scale 10]
    (doseq [x (range 64)
            y (range 32)]
      (let [disp-idx (+ (* y 64) x)
            display (:display cpu)
            on? (= 1 (nth display disp-idx))]
        (when on?
          (q/rect (* x scale) (* y scale) 10 10)))))
  (when (:paused? cpu)
    (q/fill 255 0 0)
    (q/text "PAUSED" 10 20)))

(defn -main [& args]
  (let [rom-path (or (first args) "roms/IBM Logo.ch8")]
    (q/sketch
     :title "Clojure Chip-8"
     :size [640 320] ;; (64 * 1, 32 * 10)
     :setup #(setup rom-path)
     :update update-state
     :draw draw-state
     :middleware [m/fun-mode]
     :key-pressed on-key-pressed
     :key-released on-key-released)))

;;; debugging
(defn run-headless [steps rom-path]
  (println "Starting headless run")
  (let [cpu (-> (init-cpu)
                (load-rom rom-path))]
    (reduce (fn [cpu count]
              (step cpu))
            cpu (range steps))))

(comment
  (-main "roms/test_opcode.ch8")
  (-main "roms/Airplane.ch8")
  (-main "./resources/roms/pong.ch8")
  (-main)
  (run-headless 6000 "roms/Airplane.ch8"))
