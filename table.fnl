table.fnl

;; Table Definitions
(local numC [])
(local strC [])
(local idC [])
(local appC [])
(local lamC [])
(local ifC [])

;; insert functions
;; Function to insert a numC into the table
(fn create-numC [n]
  {:n n})

;; Function to insert a strC into the table
(fn create-strC [str]
  {:str str})

;; Function to insert an idC into the table
(fn create-idC [id]
  {:id id})

;; Function to insert an idC into the table
(fn create-appC [funID argsIDs]
  {:funID funID :argsIDs argsIDs})

;; Function to insert a lamC into the table
(fn create-lamC [argsIDs body]
  {:argsIDs argsIDs :body body})

;; Function to insert an ifC into the table
(fn create-ifC [ifID thenID elseID]
  {:ifID ifID :thenID thenID :elseID elseID})

;; Insert values
(var num (create-numC 42))
(print num.n)
(create-strC "I hate fennel")
(create-idC "x") ; we have to store ids as strings becasue in Fennel symbols are interned and this would break scope
(create-appC "myFunID" ["arg1ID" "arg2ID" "arg3ID"])
(create-lamC ["arg1" "arg2"] "bodyID")
(create-ifC "true" 12 10)

;;Assume ASTs work (they don't)

; (fn interp [ast env]
;     )