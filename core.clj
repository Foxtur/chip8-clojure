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

(defn read-mem [cpu addr]
  (wrap-byte (aget ^bytes (:memory cpu) addr)))

(defn write-mem [cpu addr val]
  (aset ^bytes (:memory cpu) addr (unchecked-byte val))
  cpu) ;; return CPU so we can chain operations

(defn read-reg [cpu reg-idx]
  (get-in cpu [:v reg-idx]))

(defn write-reg [cpu reg-idx val]
  (assoc-in cpu [:v reg-idx] (wrap-byte val)))

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

(defn -main []
  (println "Chip-8 Emulator Running..."))
