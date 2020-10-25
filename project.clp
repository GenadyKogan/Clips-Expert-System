;----------------------------------------------------------------------------
; CLASSES
;----------------------------------------------------------------------------
(defclass PERSON
	(is-a USER)
	(role concrete)
	(slot company)
	(slot devicetype))

(defclass DEVICENAME
	(is-a USER)
	(slot company)
	(slot inch)
	(slot capacity)
	(slot ram)
	(slot color)
	(slot model)
	(slot suggested_device))

;----------------------------------------------------------------------------
; DEFAULT INSTANCES
;----------------------------------------------------------------------------

(definstances PERSON-INSTANCES
	(client of PERSON))

(definstances DEVICE-INSTANCES
	(which_device of DEVICENAME))

;----------------------------------------------------------------------------
; INITIAL USER INPUTS AND VALIDATIONS
;----------------------------------------------------------------------------

(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer)

; RULE TO GET THE USER INPUT
(defrule GetCompanion(declare (salience 10))
    =>
    (printout t crlf)
    (printout t "--------------------------------------------------------------------------------------------------------" crlf)
    (printout t "------------------------ WELCOME TO THE SMARTPHONES / TABLETS / LAPTOPS EXPERT ------------------------" crlf)
    (printout t "--------------------------------------------------------------------------------------------------------" crlf)
    (printout t crlf)
    (send [client] put-devicetype
    (user-input-validation "Which device do you prefer? (mobile/tablet/laptop):  "mobile tablet laptop)))

;----------------------------------------------------------------------------
; RULES OF THE EXPERT SYSTEM TO SELECT THE DEVICE
;----------------------------------------------------------------------------


;----------------------------------------------------------------------------
;								MOBILE
;----------------------------------------------------------------------------

;---------------------------------------- CHOOSING DEVICE
; RULE TO DEVICETYPE
(defrule buy_mobile
	(and ?ins <- (object (is-a DEVICENAME) )
	(object (is-a PERSON)(devicetype mobile)))
	=>
	(printout t crlf)
	(printout t "Let me select a manufacturer" crlf crlf)
	(send [which_device] put-company
   (user-input-validation "Enter your preferred company (apple/samsung/xiaomi):  "
        apple samsung xiaomi)))


;---------------------------------------- COMPANY_APPLE
(defrule mobile_apple
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (5.8/6.1/6.5):  "
        5.8 6.1 6.5)))

;---------------------------------------- INCH_APPLE
(defrule mobile_apple_inch5.8
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 5.8 inch:" crlf
	;"Apple iPhone 8 Plus 5.8 inches 64GB gold" crlf
	;"Apple iPhone 8 Plus 5.8 inches 256GB grey" crlf
	;"Apple iPhone 7 Plus 5.8 inches 128GB silver" crlf
	;"Apple iPhone 7 Plus 5.8 inches 256GB gold" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/128/256):  "
		64 128 256 )))

(defrule mobile_apple_inch6.1
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 6.1 inch:" crlf
	;"Apple iPhone 11 6.1 inches 64 GB grey" crlf
	;"Apple iPhone XR 6.1 inches 64 GB gold" crlf
	;"Apple iPhone XR 6.1 inches 128GB gold" crlf
	;"Apple iPhone 11 6.1 inches 128GB silver" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/128):  "
        64 128)))

(defrule mobile_apple_inch6.5
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.5)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 6.5 inch:" crlf
	;"Apple iPhone 11 Pro Max 6.5 inches 64GB grey" crlf
	;"Apple iPhone 11 Pro Max 6.5 inches 256GB gold" crlf
	;"Apple iPhone Xs Max 6.5 inches 64GB silver" crlf
	;"Apple iPhone Xs Max 6.5 inches 256GB gold" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/256):  "
        64 256 )))

;---------------------------------------- CAPACITY_APPLE

(defrule mobile_apple_inch5.8_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Apple iPhone 8 Plus 5.8 inches 64GB gold" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (gold):  "
       gold )))

(defrule mobile_apple_inch5.8_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Apple iPhone 7 Plus 5.8 inches 128GB silver" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (silver):  "
       silver )))

(defrule mobile_apple_inch5.8_capacity256
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 256)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 256 capacity:" crlf
	;"Apple iPhone 7 Plus 5.8 inches 256GB gold" crlf
	;"Apple iPhone 8 Plus 5.8 inches 256GB grey" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/gold):  "
       grey gold )))



(defrule mobile_apple_inch6.1_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Apple iPhone 11 6.1 inches 64 GB grey" crlf
	;"Apple iPhone XR 6.1 inches 64 GB gold" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/gold):  "
       grey gold )))

(defrule mobile_apple_inch6.1_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Apple iPhone XR 6.1 inches 128GB gold" crlf
	;"Apple iPhone 11 6.1 inches 128GB silver" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (silver/gold):  "
       silver gold )))



(defrule mobile_apple_inch6.5_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.5) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Apple iPhone 11 Pro Max 6.5 inches 64GB grey" crlf
	;"Apple iPhone Xs Max 6.5 inches 64GB silver" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/silver):  "
       grey silver )))

(defrule mobile_apple_inch6.5_capacity256
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.5) (capacity 256)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 256 capacity:" crlf
	;"Apple iPhone 11 Pro Max 6.5 inches 256GB gold" crlf
	;"Apple iPhone Xs Max 6.5 inches 256GB gold" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (gold):  "
       gold )))



;---------------------------------------- COLOR_APPLE

(defrule mobile_apple_inch5.8_capacity64_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 64) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 8 Plus 5.8 inches 64GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch5.8_capacity128_silver
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 128) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 7 Plus 5.8 inches 128GB silver" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch5.8_capacity256_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 256) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 7 Plus 5.8 inches 256GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch5.8_capacity256_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 5.8) (capacity 256) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 8 Plus 5.8 inches 256GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))



(defrule mobile_apple_inch6.1_capacity64_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1) (capacity 64) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone XR 6.1 inches 64 GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch6.1_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1) (capacity 64) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 11 6.1 inches 64 GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch6.1_capacity128_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1) (capacity 128) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone XR 6.1 inches 128GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch6.1_capacity128_silver
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.1) (capacity 128) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 11 6.1 inches 128GB silver" crlf crlf)
	(send [which_device] put-suggested_device ""))



(defrule mobile_apple_inch6.5_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.5) (capacity 64) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 11 Pro Max 6.5 inches 64GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch6.5_capacity64_silver
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.5) (capacity 64) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone Xs Max 6.5 inches 64GB silver" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_apple_inch6.5_capacity256_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company apple)(inch 6.5) (capacity 256) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple iPhone 11 Pro Max 6.5 inches 256GB gold" crlf
	"Apple iPhone Xs Max 6.5 inches 256GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))




;---------------------------------------- COMPANY_SAMSUNG
(defrule mobile_samsung
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)))
	=>
	(printout t crlf)
	(printout t "well" crlf crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (5.0/6.2/6.7):  "
        5.0 6.2 6.7)))



;---------------------------------------- INCH_SAMSUNG

(defrule mobile_samsung_inch5.0
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung) (inch 5.0)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 5.0 inch:" crlf
	;"Samsung Galaxy J2 Core 5.0 inches 128GB black" crlf
	;"Samsung Galaxy J5 5.0 inches 64GB black" crlf
	;"Samsung Galaxy Fold 5.0 inches 512GB black/grey" crlf
	;"Samsung Galaxy J7 Pro 5.0 inches 64GB gold" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/128/512):  "
		64 128 512 )))


(defrule mobile_samsung_inch6.2
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung) (inch 6.2)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 6.2 inch:" crlf
	;"Samsung Galaxy Z FOLD2 6.2 inches 256GB black" crlf
	;"Samsung Galaxy S20 6.2 inches 128GB black/grey" crlf
	;"Samsung Galaxy M20 6.2 inches 64GB black/grey" crlf
	;"Samsung Galaxy A10 6.2 inches 64GB gold/black" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/128/256):  "
		64 128 256 )))



(defrule mobile_samsung_inch6.7
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung) (inch 6.7)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 6.7 inch:" crlf
	;"Samsung Galaxy A71 6.7 inches 128GB silver" crlf
	;"Samsung Galaxy A80 6.7 inches 128GB black/gold" crlf
	;"Samsung Galaxy Note 10 Plus 6.7 inches 256GB white/gold" crlf
	;"Samsung Galaxy M51 6.7 inches 128GB black" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (128/256):  "
		128 256 )))



;---------------------------------------- CAPACITY_SAMSUNG

(defrule mobile_samsung_inch5.0_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Samsung Galaxy J5 5.0 inches 64GB black" crlf
	;"Samsung Galaxy J7 Pro 5.0 inches 64GB gold" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/gold):  "
       black gold )))

(defrule mobile_samsung_inch5.0_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Samsung Galaxy J2 Core 5.0 inches 128GB black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black):  "
       black )))

(defrule mobile_samsung_inch5.0_capacity512
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 512)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 512 capacity:" crlf
	;"Samsung Galaxy Fold 5.0 inches 512GB black/grey" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/black):  "
       grey black )))



(defrule mobile_samsung_inch6.2_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Samsung Galaxy M20 6.2 inches 64GB black/grey" crlf
	;"Samsung Galaxy A10 6.2 inches 64GB gold/black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/black/gold):  "
       grey black gold )))

(defrule mobile_samsung_inch6.2_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Samsung Galaxy S20 6.2 inches 128GB black/grey" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/black):  "
       grey black )))

(defrule mobile_samsung_inch6.2_capacity256
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 256)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 256 capacity:" crlf
	;"Samsung Galaxy Z FOLD2 6.2 inches 256GB black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black):  "
       black )))



(defrule mobile_samsung_inch6.7_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Samsung Galaxy A71 6.7 inches 128GB silver" crlf
	;"Samsung Galaxy A80 6.7 inches 128GB black/gold" crlf
	;"Samsung Galaxy M51 6.7 inches 128GB black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/gold/silver):  "
       black gold silver )))

(defrule mobile_samsung_inch6.7_capacity256
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 256)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 256 capacity:" crlf
	;"Samsung Galaxy Note 10 Plus 6.7 inches 256GB white/gold" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (white/gold):  "
       white gold )))



;---------------------------------------- COLOR_SAMSUNG

(defrule mobile_samsung_inch5.0_capacity64_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 64) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy J7 Pro 5.0 inches 64GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch5.0_capacity64_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 64) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy J5 5.0 inches 64GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch5.0_capacity128_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 128) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy J2 Core 5.0 inches 128GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch5.0_capacity512_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 512) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Fold 5.0 inches 512GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch5.0_capacity512_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 5.0) (capacity 512) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Fold 5.0 inches 512GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))




(defrule mobile_samsung_inch6.2_capacity64_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 64) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy M20 6.2 inches 64GB black" crlf
	"Samsung Galaxy A10 6.2 inches 64GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.2_capacity64_glod
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 64) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy A10 6.2 inches 64GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.2_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 64) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy M20 6.2 inches 64GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.2_capacity128_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 128) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy S20 6.2 inches 128GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.2_capacity128_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 128) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy S20 6.2 inches 128GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.2_capacity256_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.2) (capacity 256) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Z FOLD2 6.2 inches 256GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))



(defrule mobile_samsung_inch6.7_capacity128_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 128) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy M51 6.7 inches 128GB black" crlf
	"Samsung Galaxy A80 6.7 inches 128GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.7_capacity128_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 128) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy A80 6.7 inches 128GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.7_capacity128_silver
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 128) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy A71 6.7 inches 128GB silver" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.7_capacity256_gold
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 256) (color gold) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Note 10 Plus 6.7 inches 256GB gold" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_samsung_inch6.7_capacity256_white
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company samsung)(inch 6.7) (capacity 256) (color white) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Note 10 Plus 6.7 inches 256GB white" crlf crlf)
	(send [which_device] put-suggested_device ""))




;---------------------------------------- COMPANY_XIAOMI

(defrule mobile_xiaomi
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (6.3/6.7):  "
        6.3 6.7)))

;---------------------------------------- INCH_XIAOMI

(defrule mobile_xiaomi_inch6.3
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) ))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 6.3 inch:" crlf
	;"Xiaomi Redmi K20 Pro 6.3 inch 128GB blue/black" crlf
	;"Xiaomi Redmi Note 8 6.3 inch 64GB black" crlf
	;"Xiaomi Redmi K20 6.3 inch 64GB green" crlf
	;"Xiaomi Poco F1 6.3 inch 64GB black" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/128):  "
		64 128 ) ) )

(defrule mobile_xiaomi_inch6.7
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) ))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 6.7 inch:" crlf
	;"Xiaomi Poco X3 6.7 inch 128GB blue/grey" crlf
	;"Xiaomi Poco X3 6.7 inch 64GB grey" crlf
	;"Xiaomi Redmi Note 9 Pro Max 6.7 inch 64GB blue/grey" crlf
	;"Xiaomi Mi 10 6.7 inch 128GB black" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/128):  "
		64 128 ) ) )



;---------------------------------------- CAPACITY_XIAOMI

(defrule mobile_xiaomi_inch6.3_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Xiaomi Redmi Note 8 6.3 inch 64GB black" crlf
	;"Xiaomi Redmi K20 6.3 inch 64GB green" crlf
	;"Xiaomi Poco F1 6.3 inch 64GB black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (green/black):  "
       green black ) ) )

(defrule mobile_xiaomi_inch6.3_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Xiaomi Redmi K20 Pro 6.3 inch 128GB blue/black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (blue/black):  "
       blue black ) ) )


(defrule mobile_xiaomi_inch6.7_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 64 capacity:" crlf
	;"Xiaomi Poco X3 6.7 inch 64GB grey" crlf
	;"Xiaomi Redmi Note 9 Pro Max 6.7 inch 64GB blue/grey" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/blue):  "
       grey blue ) ) )

(defrule mobile_xiaomi_inch6.7_capacity128
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 128)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 128 capacity:" crlf
	;"Xiaomi Poco X3 6.7 inch 128GB blue/grey" crlf
	;"Xiaomi Mi 10 6.7 inch 128GB black" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/blue/black):  "
       grey blue black ) ) )



;---------------------------------------- COLOR_XIAOMI

(defrule mobile_xiaomi_inch6.3_capacity64_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) (capacity 64) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Redmi Note 8 6.3 inch 64GB black" crlf
	"Xiaomi Poco F1 6.3 inch 64GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.3_capacity64_green
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) (capacity 64) (color green) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Redmi K20 6.3 inch 64GB green" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.3_capacity128_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) (capacity 128) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Redmi K20 Pro 6.3 inch 128GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.3_capacity128_blue
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.3) (capacity 128) (color blue) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Redmi K20 Pro 6.3 inch 128GB blue" crlf crlf)
	(send [which_device] put-suggested_device ""))



(defrule mobile_xiaomi_inch6.7_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 64) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Poco X3 6.7 inch 64GB grey" crlf
	"Xiaomi Redmi Note 9 Pro Max 6.7 inch 64GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.7_capacity64_blue
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 64) (color blue) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Redmi Note 9 Pro Max 6.7 inch 64GB blue" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.7_capacity128_grey
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 128) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Poco X3 6.7 inch 128GB grey" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.7_capacity128_blue
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 128) (color blue) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Poco X3 6.7 inch 128GB blue" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule mobile_xiaomi_inch6.7_capacity128_black
	(and ?ins <- (object (is-a PERSON) (devicetype mobile))
	(object (is-a DEVICENAME) (company xiaomi)(inch 6.7) (capacity 128) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Xiaomi Mi 10 6.7 inch 128GB black" crlf crlf)
	(send [which_device] put-suggested_device ""))


















;----------------------------------------------------------------------------
;													LAPTOPS
;----------------------------------------------------------------------------

;---------------------------------------- CHOOSING DEVICE
; RULE TO DEVICETYPE
(defrule buy_laptop
	(and ?ins <- (object (is-a DEVICENAME) )
	(object (is-a PERSON)(devicetype laptop)))
	=>
	(printout t crlf)
	(printout t "Let me select a manufacturer" crlf crlf)
	(send [which_device] put-company
   (user-input-validation "Enter your preferred company (dell/asus/macbook):  "
        dell asus macbook)))


;---------------------------------------- COMPANY_DELL
(defrule lapt_dell
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (14/15.6/17.3):  "
        14 15.6 17.3)))

;---------------------------------------- INCH_DELL

(defrule lapt_dell_inch14
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 14)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 14 inch:" crlf
	;"Dell Inspiron 5401 N5401-6210 i5-1035G1 14 inches 512GB SSD 8GB RAM White  " crlf
	;"Dell Inspiron 5401 N5401-6210 i5-1035G1 14 inches 512GB SSD 16GB RAM Silver  " crlf
	;"Dell Vostro 5401 i5-1035G1 14 inches 512GB SSD 16GB RAM Grey  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (8/16):  "
		8 16 )))

(defrule lapt_dell_inch15.6
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 15.6)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 15.6 inch:" crlf
	;"Dell Vostro 3591 i5-1035G1 15.6 inches 256GB SSD 8GB RAM Black  " crlf
	;"Dell Vostro 3591 i5-1035G1 15.6 inches 256GB SSD 256GB 16RAM Black  " crlf
	;"Dell Inspiron 5501 i5-1035G1 15.6 inches 512GB SSD 8GB RAM Silver  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (8/16):  "
		8 16 )))

(defrule lapt_dell_inch17.3
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 17.3)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 17.3 inch:" crlf
	;"Dell Inspiron 3793 i7-1065G7 17.3 inches 128GB SSD 8GB RAM Black  " crlf
	;"Dell G7 7700 i7-10750H 17.3 inches 1TB SSD 32GB RAM Black  " crlf
	;"Dell Inspiron 3793 i5-1035G1 17.3 inches 1TB SSD 8GB RAM Black  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (8/32):  "
		8 32 )))

;---------------------------------------- RAM_DELL

(defrule lapt_dell_inch14_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 14) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Dell Inspiron 5401 N5401-6210 i5-1035G1 14 inches 512GB SSD 8GB RAM White  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (white):  "
		     white )))

(defrule lapt_dell_inch14_16ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 14) (ram 16)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 16GB RAM:" crlf
	;"Dell Inspiron 5401 N5401-6210 i5-1035G1 14 inches 512GB SSD 16GB RAM Silver " crlf
	;"Dell Vostro 5401 i5-1035G1 14 inches 512GB SSD 16GB RAM Grey  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (silver/grey):  "
		     silver grey )))




(defrule lapt_dell_inch15.6_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 15.6) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Dell Vostro 3591 i5-1035G1 15.6 inches 256GB SSD 8GB RAM Black " crlf
	;"Dell Inspiron 5501 i5-1035G1 15.6 inches 512GB SSD 8GB RAM Silver  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/silver):  "
		     black silver )))

(defrule lapt_dell_inch15.6_16ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 15.6) (ram 16)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 16GB RAM:" crlf
		;"Dell Vostro 3591 i5-1035G1 15.6 inches 256GB SSD 256GB 16RAM Black  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black):  "
		     black )))





(defrule lapt_dell_inch17.3_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 17.3) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Dell Inspiron 3793 i7-1065G7 17.3 inches 128GB SSD 8GB RAM Black  " crlf
	;"Dell Inspiron 3793 i5-1035G1 17.3 inches 1TB SSD 8GB RAM Grey  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/grey):  "
		    black grey )))

(defrule lapt_dell_inch17.3_32ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 17.3) (ram 32)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 32GB RAM:" crlf
	;"Dell G7 7700 i7-10750H 17.3 inches 1TB SSD 32GB RAM Black  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black):  "
       black )))

;---------------------------------------- COLOR_DELL

(defrule lapt_dell_inch14_8GBRAM_White
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 14) (ram 8) (color white) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Inspiron 5401 N5401-6210 i5-1035G1 14 inches 512GB SSD 8GB RAM white  " crlf crlf)
	(send [which_device] put-suggested_device ""))
	
(defrule lapt_dell_inch14_16GBRAM_silver
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 14) (ram 16) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Inspiron 5401 N5401-6210 i5-1035G1 14 inches 512GB SSD 16GB RAM silver  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_dell_inch14_16GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 14) (ram 16) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Vostro 5401 i5-1035G1 14 inches 512GB SSD 16GB RAM grey  " crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule lapt_dell_inch15.6_8GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 15.6) (ram 8) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Vostro 3591 i5-1035G1 15.6 inches 256GB SSD 8GB RAM black  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_dell_inch15.6_16GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 15.6) (ram 16) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Vostro 3591 i5-1035G1 15.6 inches 256GB SSD 256GB 16RAM black  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_dell_inch15.6_8GBRAM_silver
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 15.6) (ram 8) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Inspiron 5501 i5-1035G1 15.6 inches 512GB SSD 8GB RAM silver  " crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule lapt_dell_inch17.3_8GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 17.3) (ram 8) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Inspiron 3793 i7-1065G7 17.3 inches 128GB SSD 8GB RAM black  " crlf crlf) 
	(send [which_device] put-suggested_device ""))

(defrule lapt_dell_inch17.3_32GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 17.3) (ram 32) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell G7 7700 i7-10750H 17.3 inches 1TB SSD 32GB RAM black  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_dell_inch17.3_8GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company dell)(inch 17.3) (ram 8) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Dell Inspiron 3793 i5-1035G1 17.3 inches 1TB SSD 8GB RAM black  " crlf crlf)
	(send [which_device] put-suggested_device ""))



;---------------------------------------- COMPANY_ASUS
(defrule lapt_asus
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (11.6/14/17.3):  "
        11.6 14 17.3)))

;---------------------------------------- INCH_ASUS

(defrule lapt_asus_inch11.6
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 11.6)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 11.6 inch:" crlf
	;"Asus VivoBook E12 L203NA-FD126TS 11.6 inches 64GB SSD 4GB RAM grey  " crlf
	;"Asus Laptop E210MA-GJ082T 11.6 inches 128GB SSD 8GB RAM Blue  " crlf
	;"Asus Laptop E210MA-GJ098T 11.6 inches 128GB SSD 8GB RAM white  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (4/8):  "
		4 8 )))


((defrule lapt_asus_inch14
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 14 inch:" crlf
	;"Asus Zenbook 14 UX434FAC-A5193T 14 inches 256GB SSD 8GB RAM Blue  " crlf
	;"Asus Zenbook 14 UX434FQ-A5058T 14 inches 512GB SSD 8GB RAM silver  " crlf
	;"Asus ROG Zephyrus G14 GA401IU-HE166T 14 inches 1TB SSD 32GB RAM white  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (8/32):  "
		8 32 )))


(defrule lapt_asus_inch17.3
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 17.3)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 17.3 inch:" crlf
	;"Asus ROG Zephyrus S GX701GVR-H6042T 17.3 inches 512GB SSD 16GB RAM black  " crlf
	;"Asus ROG Strix SCAR 17 G732LWS-HG110T 17.3 inches 512TB SSD 32GB RAM grey  " crlf
	;"Asus ROG Zephyrus S17 GX701LV-HG018T 17.3 inches 1TB SSD 32GB RAM black  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (16/32):  "
		16 32 )))

;---------------------------------------- RAM_ASUS

(defrule lapt_asus_inch11.6_4ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 11.6) (capacity 4ram)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 4GB RAM:" crlf
	;"Asus VivoBook E12 L203NA-FD126TS 11.6 inches 64GB SSD 4GB RAM grey  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey):  "
		     grey )))
(defrule lapt_asus_inch11.6_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Asus Laptop E210MA-GJ082T 11.6 inches 128GB SSD 8GB RAM Blue  " crlf
	;"Asus Laptop E210MA-GJ098T 11.6 inches 128GB SSD 8GB RAM white  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (Blue/white):  "
		     blue white )))


(defrule lapt_asus_inch14_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Asus Zenbook 14 UX434FAC-A5193T 14 inches 256GB SSD 8GB RAM blue  " crlf
	;"Asus Zenbook 14 UX434FQ-A5058T 14 inches 512GB SSD 8GB RAM silver  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (Blue/silver):  "
		     blue silver )))

(defrule lapt_asus_inch14_32ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14) (ram 32)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 32GB RAM:" crlf
	;"Asus ROG Zephyrus G14 GA401IU-HE166T 14 inches 1TB SSD 32GB RAM white  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (white):  "
		     white )))


(defrule lapt_asus_inch17.3_16ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 17.3) (ram 16)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 16GB RAM:" crlf
	;"Asus ROG Zephyrus S GX701GVR-H6042T 17.3 inches 512GB SSD 16GB RAM black " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black):  "
		     black )))

(defrule lapt_asus_inch17.3_32ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 17.3) (ram 32)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 32GB RAM:" crlf
	;"Asus ROG Strix SCAR 17 G732LWS-HG110T 17.3 inches 512TB SSD 32GB RAM grey  " crlf
	;"Asus ROG Zephyrus S17 GX701LV-HG018T 17.3 inches 1TB SSD 32GB RAM black  " crlf crlf)	
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/black):  "
		   grey black )))


;---------------------------------------- COLOR_ASUS

(defrule lapt_asus_inch11.6_4GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 11.6) (ram 4) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus VivoBook E12 L203NA-FD126TS 11.6 inches 64GB SSD 4GB RAM grey  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_asus_inch11.6_8GBRAM_Blue
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 11.6) (ram 8) (color Blue) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus Laptop E210MA-GJ082T 11.6 inches 128GB SSD 8GB RAM Blue  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_asus_inch11.6_8GBRAM_white
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 11.6) (ram 8) (color white) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus Laptop E210MA-GJ082T 11.6 inches 128GB SSD 8GB RAM white  " crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule lapt_asus_inch14_8GBRAM_Blue
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14) (ram 8) (color Blue) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus Zenbook 14 UX434FAC-A5193T 14 inches 256GB SSD 8GB RAM Blue  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_asus_inch14_8GBRAM_silver
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14) (ram 8) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus Zenbook 14 UX434FQ-A5058T 14 inches 512GB SSD 8GB RAM silver  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_asus_inch14_32GBRAM_white
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 14) (ram 32) (color white) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus ROG Zephyrus G14 GA401IU-HE166T 14 inches 1TB SSD 32GB RAM white  " crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule lapt_asus_inch17.3_16GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 17.3) (ram 16) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus ROG Zephyrus S GX701GVR-H6042T 17.3 inches 512GB SSD 16GB RAM black " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_asus_inch17.3_32GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 17.3) (ram 32) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus ROG Strix SCAR 17 G732LWS-HG110T 17.3 inches 512TB SSD 32GB RAM grey " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_asus_inch17.3_32GBRAM_black
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company asus)(inch 17.3) (ram 32) (color black) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Asus ROG Zephyrus S17 GX701LV-HG018T 17.3 inches 1TB SSD 32GB RAM black " crlf crlf)
	(send [which_device] put-suggested_device ""))




;---------------------------------------- COMPANY_APPLE

(defrule lapt_macbook
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (13.3/16):  "
        13.3 16)))


;---------------------------------------- INCH_APPLE

(defrule lapt_macbook_inch13.3
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 13.3)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 13.3 inch:" crlf
	;"Apple MacBook Air 13 Early 2020 13.3 inches 256GB SSD 8GB RAM silver  " crlf
	;"Apple MacBook Pro 13 Mid 2020 13.3 inches 256GB SSD 8GB RAM grey  " crlf
	;"Apple MacBook Pro 13 Mid 2020 13.3 inches 1TB SSD 32GB RAM grey  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (8/32):  "
		8 32 )))


(defrule lapt_macbook_inch16
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 16)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 16 inch:" crlf
	;"Apple MacBook Pro 16 Late 2019 16 inches 512GB SSD 8GB RAM grey  " crlf
	;"Apple MacBook Pro 16 Late 2019 16 inches 512GB SSD 16GB RAM grey  " crlf
	;"Apple MacBook Pro 16 Late 2019 16 inches 1TB SSD 16GB RAM silver  " crlf crlf)
		(send [which_device] put-ram
	(user-input-validation "Enter your preferred RAM (8/16):  "
		8 16 )))




;---------------------------------------- RAM_APPLE

(defrule lapt_macbook_inch13.3_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 13.3) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Apple MacBook Air 13 Early 2020 13.3 inches 256GB SSD 8GB RAM grey  " crlf
	;"Apple MacBook Air 13 Early 2020 13.3 inches 256GB SSD 8GB RAM silver  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/silver):  "
		     grey silver )))

(defrule lapt_macbook_inch13.3_32ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 13.3) (ram 32)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 32GB RAM:" crlf
	;"Apple MacBook Pro 13 Mid 2020 13.3 inches 1TB SSD 32GB RAM grey " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey):  "
		     grey )))


(defrule lap_macbook_inch16_8ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 16) (ram 8)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 8GB RAM:" crlf
	;"Apple MacBook Pro 16 Late 2019 16 inches 512GB SSD 8GB RAM grey " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey):  "
		     grey )))
		     
(defrule lapt_macbook_inch16_16ram
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 16) (ram 16)))
	=>
	(printout t crlf)
	;(printout t "Result: only device with 16GB RAM:" crlf
	;"Apple MacBook Pro 16 Late 2019 16 inches 512GB SSD 16GB RAM grey  " crlf
	;"Apple MacBook Pro 16 Late 2019 16 inches 1TB SSD 16GB RAM silver  " crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/silver):  "
		     grey silver )))

;---------------------------------------- COLOR_APPLE

(defrule lapt_macbook_inch13.3_8GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 13.3) (ram 8) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple MacBook Air 13 Early 2020 13.3 inches 256GB SSD 8GB RAM grey  " crlf crlf)
	(send [which_device] put-suggested_device ""))
	
(defrule lapt_macbook_inch13.3_8GBRAM_silver
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 13.3) (ram 8) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple MacBook Air 13 Early 2020 13.3 inches 256GB SSD 8GB RAM silver  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_macbook_inch13.3_32GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 13.3) (ram 32) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple MacBook Pro 13 Mid 2020 13.3 inches 1TB SSD 32GB RAM grey  " crlf crlf)
	(send [which_device] put-suggested_device ""))



(defrule lapt_macbook_inch16_8GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 16) (ram 8) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple MacBook Pro 16 Late 2019 16 inches 512GB SSD 8GB RAM grey  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_macbook_inch16_16GBRAM_grey
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 16) (ram 16) (color grey) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple MacBook Pro 16 Late 2019 16 inches 512GB SSD 16GB RAM grey  " crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule lapt_macbook_inch16_16GBRAM_silver
	(and ?ins <- (object (is-a PERSON) (devicetype laptop))
	(object (is-a DEVICENAME) (company macbook)(inch 16) (ram 16) (color silver) ))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Apple MacBook Pro 16 Late 2019 16 inches 1TB SSD 16GB RAM silver  " crlf crlf)
	(send [which_device] put-suggested_device ""))


























;----------------------------------------------------------------------------
;								TABLET
;----------------------------------------------------------------------------
;---------------------------------------- CHOOSING DEVICE
; RULE TO DEVICETYPE
(defrule buy_tablet
	(and ?ins <- (object (is-a DEVICENAME) )
	(object (is-a PERSON)(devicetype tablet)))
	=>
	(printout t crlf)
	(printout t "Let me select a manufacturer" crlf crlf)
	(send [which_device] put-company
   (user-input-validation "Enter your preferred company (apple/samsung/lenovo):  "
        apple samsung lenovo )))


;---------------------------------------- COMPANY_SAMSUNG
(defrule tab_samsung
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (8/10.4/12.4):  "
       8 10.4 12.4)))
;---------------------------------------- INCH_SAMSUNG

(defrule tab_samsung_inch8
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)(inch 8)))
	=>
	(printout t crlf)
	(printout t "Result: only one device with 8 inch:" crlf
	"Samsung Galaxy Tab A 2GB+32GB SM-T290 - WiFi - Black - 1 Year Warranty by The Official Distributor" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_samsung_inch10.4
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)(inch 10.4)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Samsung Galaxy Tab S6 Lite 64GB SM-P610 - WiFi - Oxford Gray - 1 Year Warranty by The Official Distributor" crlf
	;"Samsung Galaxy Tab S6 Lite 64GB SM-P615 - 4G - Oxford Gray - 1 Year Warranty by The Official Distributor" crlf crlf)
	(send [which_device] put-model
	(user-input-validation "Enter your preferred model (Galaxy Tab S6 Lite 64GB SM-P610 enter x/Galaxy Tab S6 Lite 64GB SM-P615 enter y):  "
       x y)))

(defrule tab_samsung_inch12.4
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)(inch 12.4)))
	=>
	(printout t crlf)
	(printout t "Result: only one device with 12.4 inch:" crlf
	"Samsung Galaxy Tab S7 256GB SM-T970 - WIFI - Mystic Silver - 1 Year Warranty by The Official Distributor" crlf crlf)
	(send [which_device] put-suggested_device ""))

;---------------------------------------- CAPACITY_SAMSUNG
;---------------------------------------- COLOR_SAMSUNG
;---------------------------------------- MODEL_SAMSUNG
(defrule tab_samsung_inch10.4_modelx
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)(inch 10.4) (model x)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Tab S6 Lite 64GB SM-P610 - WiFi - Oxford Gray - 1 Year Warranty by The Official Distributor" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_samsung_inch10.4_modely
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company samsung)(inch 10.4) (model y)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Samsung Galaxy Tab S6 Lite 64GB SM-P615 - 4G - Oxford Gray - 1 Year Warranty by The Official Distributor"crlf crlf)
	(send [which_device] put-suggested_device ""))

;---------------------------------------- COMPANY_APPLE
(defrule tab_apple
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (7.9/10.5/12.9):  "
        7.9 10.5 12.9)))
;---------------------------------------- INCH_APPLE
(defrule tab_apple_inch12.9
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 12.9)))
	=>
	(printout t crlf)
	(printout t "Result: only one device with 12.9 inch:" crlf
	"Apple iPad Pro 2020 12.9 inch 128GB WiFi + Cellular Space Grey"crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_apple_inch10.5
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 10.5)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Space Grey" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Silver" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Gold" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Gold" crlf
	;"Apple iPad Air 2019 10.5'' 256GB WiFi + Cellular Silver" crlf crlf)
	(send [which_device] put-ram
	(user-input-validation "Enter your preferred capacity (64/256):  "
        64 256 )))

(defrule tab_apple_inch7.9
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi Gold" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi Space grey" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi Space grey" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi Gold" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Gold" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Space grey" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi + Cellular Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi + Cellular Space grey" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (64/256):  "
        64 256 )))
;---------------------------------------- CAPACITY_APPLE
(defrule tab_apple_inch10.5_capacity256
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 10.5) (capacity 256)))
	=>
	(printout t crlf)
	(printout t "Result: only one device with 256 capacity:" crlf
	"Apple iPad Air 2019 10.5'' 256GB WiFi + Cellular Silver" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_apple_inch10.5_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 10.5) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Space Grey" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Silver" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Gold" crlf
	;"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Gold" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/silver/gold):  "
       grey silver gold )))

(defrule tab_apple_inch7.9_capacity256
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 256)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi Space grey" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi Gold" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi + Cellular Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 256GB WiFi + Cellular Space grey" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/silver/gold):  "
       grey silver gold )))

(defrule tab_apple_inch7.9_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi Gold" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi Space grey" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Silver" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Gold" crlf
	;"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Space grey" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (grey/silver/gold):  "
       grey silver gold )))



;---------------------------------------- COLOR_APPLE

(defrule tab_apple_inch10.5_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 10.5) (capacity 64) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Air 2019 10.5" crlf
	"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Space Grey" crlf crlf)
	(send [which_device] put-suggested_device ))

(defrule tab_apple_inch10.5_capacity64_silver
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 10.5) (capacity 64) (color silver)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Air 2019 10.5" crlf
	"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Space Grey" crlf crlf)
	(send [which_device] put-suggested_device ))

(defrule tab_apple_inch10.5_capacity64_gold
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 10.5) (capacity 64) (color gold)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Air 2019 10.5" crlf
	"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Gold" crlf
	"Apple iPad Air 2019 10.5'' 64GB WiFi + Cellular Gold" crlf crlf)
	(send [which_device] put-suggested_device ))


(defrule tab_apple_inch7.9_capacity256_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 256) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Mini 2019 7.9" crlf
	"Apple iPad Mini 2019 7.9'' 256GB WiFi Space grey" crlf
	"Apple iPad Mini 2019 7.9'' 256GB WiFi + Cellular Space grey" crlf crlf)
	(send [which_device] put-suggested_device ))

(defrule tab_apple_inch7.9_capacity256_silver
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 256) (color silver)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Mini 2019 7.9" crlf
	"Apple iPad Mini 2019 7.9'' 256GB WiFi Silver" crlf
	"Apple iPad Mini 2019 7.9'' 256GB WiFi + Cellular Silver" crlf crlf)
	(send [which_device] put-suggested_device ))

(defrule tab_apple_inch7.9_capacity256_gold
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 256) (color gold)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Mini 2019 7.9" crlf
	"Apple iPad Mini 2019 7.9'' 256GB WiFi Gold"  crlf crlf)
	(send [which_device] put-suggested_device ))


(defrule tab_apple_inch7.9_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 64) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Mini 2019" crlf
	"Apple iPad Mini 2019 7.9'' 64GB WiFi Space grey" crlf
	"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Space grey" crlf crlf)
	(send [which_device] put-suggested_device ))

(defrule tab_apple_inch7.9_capacity64_silver
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 64) (color silver)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Mini 2019" crlf
	"Apple iPad Mini 2019 7.9'' 64GB WiFi Silver" crlf
	"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Silver"  crlf crlf)
	(send [which_device] put-suggested_device ))

(defrule tab_apple_inch7.9_capacity64_gold
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company apple)(inch 7.9) (capacity 64) (color gold)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: iPad Mini 2019" crlf
	"Apple iPad Mini 2019 7.9'' 64GB WiFi Gold" crlf
	"Apple iPad Mini 2019 7.9'' 64GB WiFi + Cellular Gold"  crlf crlff)
	(send [which_device] put-suggested_device ))

;---------------------------------------- LENOVO
; RULE TO LENOVO
(defrule tab_lenovo
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)))
	=>
	(printout t crlf)
	(send [which_device] put-inch
	(user-input-validation "Enter your preferred inch (7/8/10.1):  "
        7 8 10.1)))
;---------------------------------------- INCH_LENOVO
; RULE TO LENOVO/INCH
(defrule tab_lenovo_inch10.1
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0004IL - Black - WiFi" crlf
	;"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0102IL - White - WiFi" crlf
	;"Lenovo TAB M10 TB-X605F 16GB ZA480001IL - Black - WiFi" crlf
	;"Lenovo TAB M10 HD TB-X505L 16GB ZA4H0039IL - Black - 4G" crlf
	;"Lenovo TAB M10 HD TB-X505L 16GB ZA4H0038IL - White - 4G" crlf
	;"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0099IL - White - 4G" crlf
	;"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0098IL - Black - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 16GB ZA490020IL - Black - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 16GB ZA490096IL - White - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 32GB ZA490085IL - White - 4G" crlf
	;"Lenovo TAB P10 TB-X705F 32GB ZA440076IL - Black - WiFi" crlf
	;"Lenovo TAB P10 TB-X705L 32GB ZA450036IL - Black - 4G" crlf
	;"Lenovo TAB P10 TB-X705L 32GB ZA450100IL - White - 4G" crlf
	;"Lenovo TAB P10 TB-X705L 64GB ZA450080IL - Black - 4G" crlf
	;"Lenovo TAB P10 TB-X705L 64GB ZA450122IL - White - 4G" crlf
	;"Lenovo Yoga Smart Tab YT-X705F 32GB ZA3V0043IL - grey - WiFi" crlf
	;"Lenovo Yoga Smart Tab YT-X705L 32GB ZA530050IL - grey - 4G" crlf
	;"Lenovo Yoga Smart Tab YT-X705F 64GB ZA3V0024IL - grey - WiFi" crlf
	;"Lenovo Yoga Smart Tab YT-X705L 64GB ZA530051IL - grey - 4G" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (16/32/64):  "
        16 32 64)))

(defrule tab_lenovo_inch7
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB E7 TB-7104I 16GB ZA410053IL - Black - 3G" crlf
	;"Lenovo TAB M7 TB-7305F 16GB ZA550247IL - grey - WiFi" crlf
	;"Lenovo TAB M7 TB-7305F 16GB ZA550246IL - Black - WiFi" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570022IL - Black - 4G" crlf
	;"Lenovo TAB M7 TB-7305F 32GB ZA550241IL - grey - WiFi" crlf
	;"Lenovo TAB M7 TB-7305F 32GB ZA550236IL - Iron grey - WiFi" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570181IL - grey - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570180IL - Black - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 32GB ZA570175IL - grey - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 32GB ZA570163IL - grey - 4G" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (16/32):  "
        16 32)))

(defrule tab_lenovo_inch8
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 8)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M8 TB-8505F 32GB ZA5G0146IL - Platinum grey - WiFi" crlf
	;"Lenovo TAB M8 TB-8505F 32GB ZA5G0145IL - Iron grey - WiFi" crlf
	;"Lenovo TAB 4 TB-8504X 16GB ZA2D0042IL - Black - 4G" crlf
	;"Lenovo TAB M8 TB-8705F 32GB ZA5F0001IL - Platinum grey - WiFi" crlf
	;"Lenovo TAB M8 TB-8505X 32GB ZA5H0128IL - Iron grey - 4G" crlf
	;"Lenovo TAB M8 TB-8505X 32GB ZA5H0129IL - Platinum grey - 4G" crlf crlf)
	(send [which_device] put-capacity
	(user-input-validation "Enter your preferred capacity (16/32):  "
        16 32)))
;---------------------------------------- CAPACITY_LENOVO
; RULE TO LENOVO/INCH/CAPACITY

(defrule tab_lenovo_inch10.1_capacity64
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 64)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB P10 TB-X705L 64GB ZA450080IL - Black - 4G" crlf
	;"Lenovo TAB P10 TB-X705L 64GB ZA450122IL - White - 4G" crlf
	;"Lenovo Yoga Smart Tab YT-X705F 64GB ZA3V0024IL - grey - WiFi" crlf
	;"Lenovo Yoga Smart Tab YT-X705L 64GB ZA530051IL - grey - 4G" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/white/grey):  "
        black white grey)))

(defrule tab_lenovo_inch10.1_capacity32
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0099IL - White - 4G" crlf
	;"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0098IL - Black - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 32GB ZA490085IL - White - 4G" crlf
	;"Lenovo TAB P10 TB-X705F 32GB ZA440076IL - Black - WiFi" crlf
	;"Lenovo TAB P10 TB-X705L 32GB ZA450036IL - Black - 4G" crlf
	;"Lenovo TAB P10 TB-X705L 32GB ZA450100IL - White - 4G" crlf
	;"Lenovo Yoga Smart Tab YT-X705F 32GB ZA3V0043IL - grey - WiFi" crlf
	;"Lenovo Yoga Smart Tab YT-X705L 32GB ZA530050IL - grey - 4G" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/white/grey):  "
        black white grey)))

(defrule tab_lenovo_inch10.1_capacity16
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 16)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0004IL - Black - WiFi" crlf
	;"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0102IL - White - WiFi" crlf
	;"Lenovo TAB M10 TB-X605F 16GB ZA480001IL - Black - WiFi" crlf
	;"Lenovo TAB M10 HD TB-X505L 16GB ZA4H0039IL - Black - 4G" crlf
	;"Lenovo TAB M10 HD TB-X505L 16GB ZA4H0038IL - White - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 16GB ZA490020IL - Black - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 16GB ZA490096IL - White - 4G" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/white):  "
        black white)))

(defrule tab_lenovo_inch8_capacity32
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 8) (capacity 32)))
	=>
	(printout t crlf)
	(printout t "Result: only grey color devices and one type of model - TAB M8 TB-8505:" crlf
	"Lenovo TAB M8 TB-8505F 32GB ZA5G0146IL - Platinum grey - WiFi" crlf
	"Lenovo TAB M8 TB-8505F 32GB ZA5G0145IL - Iron grey - WiFi" crlf
	"Lenovo TAB M8 TB-8705F 32GB ZA5F0001IL - Platinum grey - WiFi" crlf
	"Lenovo TAB M8 TB-8505X 32GB ZA5H0128IL - Iron grey - 4G" crlf
	"Lenovo TAB M8 TB-8505X 32GB ZA5H0129IL - Platinum grey - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch8_capacity16
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 8) (capacity 16)))
	=>
	(printout t crlf)
	(printout t "Result: only one device:" crlf
	"Lenovo TAB 4 TB-8504X 16GB ZA2D0042IL - Black - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch7_capacity16
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 16)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB E7 TB-7104I 16GB ZA410053IL - Black - 3G" crlf
	;"Lenovo TAB M7 TB-7305F 16GB ZA550247IL - grey - WiFi" crlf
	;"Lenovo TAB M7 TB-7305F 16GB ZA550246IL - Black - WiFi" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570022IL - Black - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570181IL - grey - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570180IL - Black - 4G" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/grey):  "
       black grey)))

(defrule tab_lenovo_inch7_capacity32
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 32)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M7 TB-7305F 32GB ZA550241IL - grey - WiFi" crlf
	;"Lenovo TAB M7 TB-7305F 32GB ZA550236IL - grey - WiFi" crlf
	;"Lenovo TAB M7 TB-7305X 32GB ZA570175IL - grey - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 32GB ZA570163IL - grey - 4G" crlf crlf)
	(send [which_device] put-color
	(user-input-validation "Enter your preferred color (black/grey):  "
       black grey)))
;---------------------------------------- COLOR_LENOVO

(defrule tab_lenovo_inch10.1_capacity64_white
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 64) (color white)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: TAB P10 TB-X705" crlf
	"Lenovo TAB P10 TB-X705L 64GB ZA450122IL - White - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch10.1_capacity64_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 64) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: Yoga Smart Tab YT-X705" crlf
	"Lenovo Yoga Smart Tab YT-X705F 64GB ZA3V0024IL - grey - WiFi" crlf
	"Lenovo Yoga Smart Tab YT-X705L 64GB ZA530051IL - grey - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity64_black
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 64) (color black)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: TAB P10 TB-X705" crlf
	"Lenovo TAB P10 TB-X705L 64GB ZA450080IL - Black - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity32_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one type of model: YOGA SMART TAB YT-X705" crlf
	"Lenovo Yoga Smart Tab YT-X705F 32GB ZA3V0043IL - grey - WiFi" crlf
	"Lenovo Yoga Smart Tab YT-X705L 32GB ZA530050IL - grey - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity32_white
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color white)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0099IL - White - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 32GB ZA490085IL - White - 4G" crlf
	;"Lenovo TAB P10 TB-X705L 32GB ZA450100IL - White - 4G" crlf crlf)
	(send [which_device] put-model
	(user-input-validation "Enter your preferred model (TAB M10 HD TB-X505 enter x/TAB M10 TB-X605 enter y/TAB P10 TB-X705 enter z):  "
        x y z)))

(defrule tab_lenovo_inch10.1_capacity32_black
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color black)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0098IL - Black - 4G" crlf
	;"Lenovo TAB P10 TB-X705F 32GB ZA440076IL - Black - WiFi" crlf
	;"Lenovo TAB P10 TB-X705L 32GB ZA450036IL - Black - 4G" crlf crlf)
	(send [which_device] put-model
	(user-input-validation "Enter your preferred model (TAB M10 HD TB-X505 enter x/TAB P10 TB-X705 enter y):  "
       x y)))

(defrule tab_lenovo_inch10.1_capacity16_white
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 16) (color white)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0102IL - White - WiFi" crlf
	;"Lenovo TAB M10 HD TB-X505L 16GB ZA4H0038IL - White - 4G" crlf
	;"Lenovo TAB M10 TB-X605L 16GB ZA490096IL - White - 4G" crlf crlf)
	(send [which_device] put-model
	(user-input-validation "Enter your preferred model (TAB M10 HD TB-X505 enter x/TAB M10 TB-X605 enter y):  "
		x y)))


(defrule tab_lenovo_inch7_capacity32_black
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 32) (color black)))
	=>
	(printout t crlf)
	(printout t "Device out of stock" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch7_capacity32_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 32) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one model: TAB M7 TB-7305" crlf
	"Lenovo TAB M7 TB-7305F 32GB ZA550241IL - grey - WiFi" crlf
	"Lenovo TAB M7 TB-7305F 32GB ZA550236IL - grey - WiFi" crlf
	"Lenovo TAB M7 TB-7305X 32GB ZA570175IL - grey - 4G" crlf
	"Lenovo TAB M7 TB-7305X 32GB ZA570163IL - grey - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch7_capacity16_grey
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 16) (color grey)))
	=>
	(printout t crlf)
	(printout t "Result: only one model: TAB M7 TB-7305" crlf
	"Lenovo TAB M7 TB-7305F 16GB ZA550247IL - grey - WiFi" crlf
	"Lenovo TAB M7 TB-7305X 16GB ZA570181IL - grey - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch7_capacity16_black
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 16) (color black)))
	=>
	(printout t crlf)
	;(printout t "Result:" crlf
	;"Lenovo TAB E7 TB-7104I 16GB ZA410053IL - Black - 3G" crlf
	;"Lenovo TAB M7 TB-7305F 16GB ZA550246IL - Black - WiFi" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570022IL - Black - 4G" crlf
	;"Lenovo TAB M7 TB-7305X 16GB ZA570180IL - Black - 4G" crlf crlf)
	(send [which_device] put-model
	(user-input-validation "Enter your preferred model (TAB E7 TB-7104: enter x/ TAB M7 TB-7305: enter y):  "
      x y)))

;---------------------------------------- MODEL_LENOVO

(defrule tab_lenovo_inch10.1_capacity32_white_TAB_M10_HD_TB-X505
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color white) (model x)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0099IL - White - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity32_white_TAB_M10_TB-X605
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color white) (model y)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 TB-X605L 32GB ZA490085IL - White - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch10.1_capacity32_white_TAB_P10_TB-X705
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color white) (model z)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB P10 TB-X705L 32GB ZA450100IL - White - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch10.1_capacity32_black_TAB_M10_HD_TB-X505
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color black) (model x)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 HD TB-X505L 32GB ZA4H0098IL - Black - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity32_black_TAB_P10_TB-X705
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 32) (color black) (model y)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB P10 TB-X705F 32GB ZA440076IL - Black - WiFi" crlf
	"Lenovo TAB P10 TB-X705L 32GB ZA450036IL - Black - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch10.1_capacity16_white_TAB_M10_HD_TB-X505
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 16) (color white) (model x)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0102IL - White - WiFi" crlf
	"Lenovo TAB M10 HD TB-X505L 16GB ZA4H0038IL - White - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity16_white_TAB_M10_TB-X605
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 16) (color white) (model y)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 TB-X605L 16GB ZA490096IL - White - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch10.1_capacity16_black_TAB_M10_TB-X605
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 16) (color black)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 TB-X605F 16GB ZA480001IL - Black - WiFi" crlf
	"Lenovo TAB M10 TB-X605L 16GB ZA490020IL - Black - 4G" crlf crlf)
	(send [which_device] put-suggested_device ""))


(defrule tab_lenovo_inch10.1_capacity16_black_TAB_M10_HD_TB-X505
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 10.1) (capacity 16) (color black) (model x)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M10 HD TB-X505F 16GB ZA4G0004IL - Black - WiFi" crlf crlf)
	(send [which_device] put-suggested_device ""))

(defrule tab_lenovo_inch7_capacity16_black_TAB_M7_TB-7305
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 16) (color black) (model x)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB E7 TB-7104I 16GB ZA410053IL - Black - 3G" crlf crlf)
	(send [which_device] put-suggested_device " "))

(defrule tab_lenovo_inch7_capacity16_black_TAB_E7_TB-7104
	(and ?ins <- (object (is-a PERSON) (devicetype tablet))
	(object (is-a DEVICENAME) (company lenovo)(inch 7) (capacity 16) (color black) (model y)))
	=>
	(printout t crlf)
	(printout t "Result:" crlf
	"Lenovo TAB M7 TB-7305F 16GB ZA550246IL - Black - WiFi" crlf
	"Lenovo TAB M7 TB-7305X 16GB ZA570022IL - Black - 4G" crlf
	"Lenovo TAB M7 TB-7305X 16GB ZA570180IL - Black - 4G" crlf crlf)
	(send [which_device] put-suggested_device " "))


;========================================================================================================
;----------------------------------------------------------------------------
; PRINTS THE FINAL SUGGESSION
;----------------------------------------------------------------------------

; RULE TO PRINT THE SUGGESTED DEVICE
(defrule choose_device (declare (salience -1))
	(object (is-a DEVICENAME) (suggested_device ?mov))
	=>
	(printout t crlf)
	(printout t "-------------------------------------------------------------------------------------------------------------------------------" crlf)
	;(printout t "" ?mov crlf)
    (printout t "-------------------------------------------------------------------------------------------------------------------------------" crlf))
