(ns chip8.core-test
  (:require [chip8.core :refer :all] ;; Easier for now to refer all
            [clojure.test :refer [deftest is testing]]))

(defn- write-op
  "Writes an `op` to the `cpu` memory starting at `addr`"
  [cpu addr op]
  (-> cpu
      (write-mem addr (bit-shift-right op 8)) ;; Write high byte
      (write-mem (inc addr) (bit-and op 0xFF)))) ;; Write low byte

(deftest cpu-storage-test
  (testing "Memory unsigned wrapping"
    (let [cpu (init-cpu)
          cpu (write-mem cpu 0x200 0xFB)]
      (is (= (read-mem cpu 0x200) 251))))

  (testing "Register 8-bit wrapping"
    (let [cpu (init-cpu)
          cpu (write-reg cpu 1 300)]
      (is (= (read-reg cpu 1) 44)))))

(deftest decoding-test
  (testing "Extracting parts from opcode 0x8AB1"
    (let [decoded (decode-opcode 0x8AB1)]
      (is (= (:op decoded) 0x8000))
      (is (= (:x decoded) 0xA))
      (is (= (:y decoded) 0xB))
      (is (= (:n decoded) 1))
      (is (= (:nn decoded) 0xB1))
      (is (= (:nnn decoded) 0xAB1)))))

(deftest pc-stack-test
  (testing "PC manipulation"
    (let [cpu (init-cpu)]
      (is (= (:pc (increment-pc cpu)) 0x202))
      (is (= (:pc (jump-pc cpu 0xABC)) 0xABC))))

  (testing "Stack operations"
    (let [cpu (init-cpu)
          cpu (push-stack cpu 0x300)
          [new-cpu addr] (pop-stack cpu)]
      (is (= addr 0x300)
          (= (count (:stack new-cpu)) 0))))

  (testing "Fetching opcodes"
    (let [cpu (-> (init-cpu)
                  (write-mem 0x200 0xAB)
                  (write-mem 0x201 0xCD))]
      (is (= (fetch-opcode cpu) 0xABCD)))))

(deftest font-loading-test
  (testing "Font is loaded into the beginning of memory")
  (let [cpu (init-cpu)]
    ;; Test first byte of '0'
    (is (= (read-mem cpu 0) 0xF0))
    ;; Test last byte of '0'
    (is (= (read-mem cpu 4) 0xF0))
    ;; Test first byte of '1'
    (is (= (read-mem cpu 5) 0x20))
    ;; Test last byte of 'F'
    (is (= (read-mem cpu 79) 0x80))))

(deftest rom-loading-test
  (testing "ROM is loaded into memory starting at 0x200"
    (let [cpu (init-cpu)
          ;; Requires a dummy file 'test-rom.ch8'
          ;; containing the bytes [0xAB 0xCD]
          cpu-with-rom (load-rom cpu "test-rom.ch8")]
      (is (= (read-mem cpu-with-rom 0x200) 0xAB))
      (is (= (read-mem cpu-with-rom 0x201) 0xCD)))))

(deftest step-test
  (testing "Step handles a Jump instruction (1NNN)"
    (let [cpu (-> (init-cpu)
                  (write-mem 0x200 0x1A)
                  (write-mem 0x201 0xBC)) ;; Opcode 0x1ABC
          new-cpu (step cpu)]
      (is (= (:pc new-cpu) 0xABC))))

  (testing "Step increments PC for unknown/unimplemented instructions"
    (let [cpu (init-cpu) ;; PC starts at 0x200
          new-cpu (step cpu)]
      (is (= (:pc new-cpu) 0x202)))))

(deftest register-instructions-test
  (testing "6XNN: Set Register"
    (let [cpu (-> (init-cpu)
                  (write-mem 0x200 0x61)
                  (write-mem 0x201 0xAF)) ;; V1 = 0xAF
          new-cpu (step cpu)]
      (is (= (read-reg new-cpu 1) 0xAF))))

  (testing "7XNN: Add to Register"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x10)
                  (write-mem 0x200 0x71)
                  (write-mem 0x201 0x01)) ;; V1 = V1 + 1
          new-cpu (step cpu)]
      (is (= (read-reg new-cpu 1) 0x11))))

  (testing "ANNN: Set Index Register"
    (let [cpu (-> (init-cpu)
                  (write-mem 0x200 0xA1)
                  (write-mem 0x201 0x23)) ;; I = 0x123
          new-cpu (step cpu)]
      (is (= (:i new-cpu) 0x123)))))

(deftest subroutine-test
  (testing "2NNN and 00EE: Call and Return"
    (let [cpu (-> (init-cpu)
                  (write-mem 0x200 0x23)
                  (write-mem 0x201 0x00) ;; CALL 0x300
                  (write-mem 0x300 0x00)
                  (write-mem 0x301 0xEE)) ;; RET
          ;; Step 1: Execute CALL
          cpu-after-call (step cpu)
          ;; Step 2: Execute RET
          cpu-after-ret (step cpu-after-call)]

      (is (= (:pc cpu-after-call) 0x300) "Should jump to 0x300")
      (is (= (first (:stack cpu-after-call)) 0x202) "Should save NEXT instr on stack")
      (is (= (:pc cpu-after-ret) 0x202) "Should return to address after CALL"))))

(deftest skip-instructions-test
  (testing "3XNN: Skip if Equal"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0xAB)
                  (write-mem 0x200 0x31)
                  (write-mem 0x201 0xAB)) ;; SE V1, 0xAB
          new-cpu (step cpu)]
      (is (= (:pc new-cpu) 0x204) "Should skip (PC + 4 total)")))

  (testing "4XNN: Skip if Not Equal"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0xAB)
                  (write-mem 0x200 0x41)
                  (write-mem 0x201 0x00)) ;; SNE V1, 0x00
          new-cpu (step cpu)]
      (is (= (:pc new-cpu) 0x204) "Should skip because 0xAB != 0x00"))))

(deftest skip-reg-instructions-test
  (testing "5XY0: Skip if registers equal"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x22)
                  (write-reg 2 0x22)
                  (write-mem 0x200 0x51)
                  (write-mem 0x201 0x20)) ;; SE V1, V2
          new-cpu (step cpu)]
      (is (= (:pc new-cpu) 0x204))))

  (testing "9XY0: Skip if registers not equal"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x22)
                  (write-reg 2 0x33)
                  (write-mem 0x200 0x91)
                  (write-mem 0x201 0x20)) ;; SNE V1, V2
          new-cpu (step cpu)]
      (is (= (:pc new-cpu) 0x204)))))

(deftest arithmetic-basic-test
  (let [cpu (-> (init-cpu)
                (write-reg 1 0x0F)
                (write-reg 2 0xF0))]

    (testing "8XY0: Assignment"
      (let [res (step (write-op cpu 0x200 0x8120))] ;; LD V1, V2
        (is (= (read-reg res 1) 0xF0))))

    (testing "8XY1: OR"
      (let [res (step (write-op cpu 0x200 0x8121))] ;; OR V1, V2
        (is (= (read-reg res 1) 0xFF))))

    (testing "8XY2: AND"
      (let [res (step (write-op cpu 0x200 0x8122))] ;; AND V1, V2
        (is (= (read-reg res 1) 0x00))))))

(deftest arithmetic-carry-test
  (testing "8XY4: Add with carry"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0xFE)
                  (write-reg 2 0x05)
                  (write-op 0x200 0x8124))
          res (step cpu)]
      (is (= (read-reg res 1) 0x03)) ;; (254 + 5) % 256
      (is (= (read-reg res 15) 1) "VF should be 1")))

  (testing "8XY5: Subtract with borrow"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x05)
                  (write-reg 2 0x02)
                  (write-op 0x200 0x8125))
          res (step cpu)]
      (is (= (read-reg res 1) 0x03))
      (is (= (read-reg res 15) 1) "VF should be 1 (No borrow)"))))

(deftest arithmetic-shift-test
  (testing "8XY6: Shift Right"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 2r00001011) ;; 11 in decimal
                  (write-op 0x200 0x8106))
          res (step cpu)]
      (is (= (read-reg res 1) 2r00000101)) ;; 5 in decimal
      (is (= (read-reg res 15) 1) "VF should be 1 (the bit dropped)"))

    (testing "8XYE: Shift Left"
      (let [cpu (-> (init-cpu)
                    (write-reg 1 2r10000001) ;; 129 in decimal
                    (write-op 0x200 0x810E))
            res (step cpu)]
        (is (= (read-reg res 1) 2r00000010)) ;; 2 in decimal (after wrapping)
        (is (= (read-reg res 15) 1) "VF should be 1 (the bit dropped)")))))

(deftest arithmetic-subn-test
  (testing "8XY7: Subtract reversed (Vy - Vx)"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x02) ;; Vx
                  (write-reg 2 0x05) ;; Vy
                  (write-op 0x200 0x8127))
          res (step cpu)]
      (is (= (read-reg res 1) 0x03)) ;; 5 - 2
      (is (= (read-reg res 15) 1) "VF should be 1 (No borrow)"))))

(deftest timers-index-test
  (testing "FX07 & FX15: Delay Timer operations"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x20)
                  (write-op 0x200 0xF115) ;; Set DT to V1 (0x20)
                  (step)
                  (write-op 0x202 0xF207) ;; Set V2 to DT
                  (step))]
      (is (= (:delay cpu) 0x20))
      (is (= (read-reg cpu 2) 0x20))))

  (testing "FX1E: Add to Index Register"
    (let [cpu (-> (init-cpu)
                  (assoc :i 0x100)
                  (write-reg 1 0x05)
                  (write-op 0x200 0xF11E)
                  (step))]
      (is (= (:i cpu) 0x105)))))

(deftest graphics-clear-test
  (testing "00E0: Clear Screen"
    (let [;; Create a CPU with a "dirty" display (first pixel is 1)
          cpu (assoc (init-cpu) :display (assoc (vec (repeat 2048 0)) 0 1))
          _ (is (= (get (:display cpu) 0) 1)) ;; Verify it's dirty

          ;; Execute CLS
          new-cpu (step (write-op cpu 0x200 0x00E0))]
      (is (= (reduce + (:display new-cpu)) 0) "All pixels should be 0"))))

(deftest graphics-draw-test
  (testing "DXYN: Drawing a single pixel sprite"
    (let [;; A sprite that is just one pixel (top-left bit of 0x80)
          cpu (-> (init-cpu)
                  (write-mem 0x300 0x80) ;; Sprite data: 10000000
                  (assoc :i 0x300)
                  (write-reg 1 0)        ;; X coord
                  (write-reg 2 0)        ;; Y coord
                  (write-op 0x200 0xD121)) ;; Draw V1, V2, height 1
          res (step cpu)]
      (is (= (nth (:display res) 0) 1) "Pixel at (0,0) should be 1")
      (is (= (read-reg res 15) 0) "No collision should have occurred")))

  (testing "DXYN: Collision detection"
    (let [cpu (-> (init-cpu)
                  (assoc :display (assoc (vec (repeat 2048 0)) 0 1)) ;; Pixel (0,0) is ON
                  (write-mem 0x300 0x80)
                  (assoc :i 0x300)
                  (write-reg 1 0)
                  (write-reg 2 0)
                  (write-op 0x200 0xD121))
          res (step cpu)]
      (is (= (nth (:display res) 0) 0) "Pixel (0,0) should be XORed to 0")
      (is (= (read-reg res 15) 1) "VF should be 1 (Collision)"))))

(deftest input-test
  (testing "EX9E: Skip if pressed"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x5)
                  (assoc :keypad #{0x5}) ;; Key 5 is down
                  (write-op 0x200 0xE19E))
          res (step cpu)]
      (is (= (:pc res) 0x204))))

  (testing "EXA1: Skip if not pressed"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0x5)
                  (assoc :keypad #{0xA}) ;; Only Key A is down
                  (write-op 0x200 0xE1A1))
          res (step cpu)]
      (is (= (:pc res) 0x204)))))

(deftest timer-logic-test
  (testing "Timers decrement but stop at zero"
    (let [cpu (-> (init-cpu)
                  (assoc :delay 2 :sound 1)
                  (decrement-timers))]
      (is (= (:delay cpu) 1))
      (is (= (:sound cpu) 0))
      (let [cpu-zero (decrement-timers cpu)]
        (is (= (:delay cpu-zero) 0))
        (is (= (:sound cpu-zero) 0))))))

(deftest blocking-key-test
  (testing "FX0A waits for a key"
    (let [cpu (-> (init-cpu)
                  (write-op 0x200 0xF10A))
          ;; No key is pressed
          res-blocked (step cpu)
          ;; Key 5 is pressed
          res-pressed (step (assoc cpu :keypad #{5}))]
      (is (= (:pc res-blocked) 0x200) "PC should not move while waiting")
      (is (= (:pc res-pressed) 0x202) "PC moves once key is found")
      (is (= (read-reg res-pressed 1) 5)))))

(deftest misc-opcodes-test
  (testing "FX29: Font address"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0xA) ;; Digit 'A'
                  (write-op 0x200 0xF129)
                  (step))]
      ;; Digit 'A' is the 11th char (index 10). 
      ;; Each char is 5 bytes. 10 * 5 = 50.
      (is (= (:i cpu) 50))))

  (testing "FX33: BCD Conversion"
    (let [cpu (-> (init-cpu)
                  (assoc :i 0x400)
                  (write-reg 1 123)
                  (write-op 0x200 0xF133)
                  (step))]
      (is (= (read-mem cpu 0x400) 1))
      (is (= (read-mem cpu 0x401) 2))
      (is (= (read-mem cpu 0x402) 3))))

  (testing "FX55: Store registers in memory"
    (let [cpu (-> (init-cpu)
                  (assoc :i 0x400)
                  (write-reg 0 0)
                  (write-reg 1 1)
                  (write-reg 2 2)
                  (write-op 0x200 0xF255)
                  step)]
      (is (= (read-mem cpu 0x400) 0))
      (is (= (read-mem cpu 0x401) 1))
      (is (= (read-mem cpu 0x402) 2)))))

(deftest fx65-test
  (testing "FX65: Memory Load"
    (let [cpu (-> (init-cpu)
                  (assoc :i 0x400)
                  (write-mem 0x400 0xAA)
                  (write-mem 0x401 0xBB)
                  (write-op 0x200 0xF165) ;; Load V0 and V1
                  (step))]
      (is (= (read-reg cpu 0) 0xAA))
      (is (= (read-reg cpu 1) 0xBB)))))

(deftest arithmetic-wrap-test
  (testing "7XNN must wrap silently"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 255)
                  (write-op 0x200 0x7101) ;; Add 1 to V1
                  (step))]
      (is (= (read-reg cpu 1) 0))))

  (testing "3XNN skip logic"
    (let [cpu (-> (init-cpu)
                  (write-reg 1 0xAA)
                  (write-op 0x200 0x31AA) ;; Skip if V1 == 0xAA
                  (step))]
      ;; PC starts at 0x200. Step increments to 0x202. 
      ;; Skip should increment to 0x204.
      (is (= (:pc cpu) 0x204)))))

(deftest random-and-jump-test
  (testing "CXNN: Random"
    (let [cpu (-> (init-cpu)
                  (write-op 0x200 0xC100) ;; RND V1, 0x00 (should always be 0)
                  (step))]
      (is (= (read-reg cpu 1) 0))))

  (testing "BNNN: Jump with offset"
    (let [cpu (-> (init-cpu)
                  (write-reg 0 0x02)
                  (write-op 0x200 0xB500) ;; Jump to 0x500 + V0
                  (step))]
      (is (= (:pc cpu) 0x502)))))

(deftest random-and-jump-test
  (testing "CXNN: Random"
    (let [cpu (-> (init-cpu)
                  (write-op 0x200 0xC100) ;; RND V1, 0x00 (should always be 0)
                  (step))]
      (is (= (read-reg cpu 1) 0))))

  (testing "BNNN: Jump with offset"
    (let [cpu (-> (init-cpu)
                  (write-reg 0 0x02)
                  (write-op 0x200 0xB500) ;; Jump to 0x500 + V0
                  (step))]
      (is (= (:pc cpu) 0x502)))))
