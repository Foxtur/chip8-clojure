(ns chip8.core
  (:require [clojure.java.io :as io]))

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
       :keypad (set nil)} ;; currently pressed keys
      (load-font)))

(defn load-rom [cpu filename]
  (let [rom-bytes
        (-> (io/resource "test-rom.ch8")
            (io/input-stream)
            (.readAllBytes))]
    (copy-to-array! rom-bytes (:memory cpu) 0x200)
    cpu))

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

(reduce (fn [a v] (if (< a 100) (+ a v) (reduced :big))) (range 20))

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

(defn decode-opcode [opcode]
  {:op (bit-and opcode 0xF000) ;; First nibble
   :x  (bit-shift-right (bit-and opcode 0x0F00) 8) ;; Second nibble
   :y  (bit-shift-right (bit-and opcode 0x00F0) 4) ;; Third nibble
   :n  (bit-and opcode 0x000F) ;; Last nibble
   :nn (bit-and opcode 0x00FF) ;; Last byte
   :nnn (bit-and opcode 0x0FFF)}) ;; Last 12 byte

(defn increment-pc [cpu]
  (update cpu :pc + 2))

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
        ;; Default: move to next instruction
        cpu-stepped (increment-pc cpu)]
    (case op
      0x0000 (case nn
               ;; CLS
               0xE0 (assoc cpu-stepped :display (vec (repeat 2048 0)))
               0xEE (let [[popped-cpu addr] (pop-stack cpu-stepped)]
                      (assoc popped-cpu :pc addr))
               cpu-stepped)
      0x1000 (assoc cpu-stepped :pc nnn)
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
      ;; DRW Vx, Vy, nibble
      0xD000 (let [vx (read-reg cpu-stepped x)
                   vy (read-reg cpu-stepped y)]
               (loop [cpu cpu-stepped
                      row 0
                      addr (:i cpu-stepped)]
                 (if (<= row (dec n))
                   (recur (write-sprite cpu-stepped vx (+ vy row) addr)
                          (inc row)
                          (inc addr))
                   cpu)))
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
                      (update cpu-stepped :i + vx)))
      ;; Default case for unimplemented opcodes
      cpu-stepped)))

(defn -main []
  (println "Chip-8 Emulator Running..."))
