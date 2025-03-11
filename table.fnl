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
(fn create-numC [id n]
  (table.insert numC {:id id :n n}))

;; Function to insert a strC into the table
(fn create-strC [id s]
  (table.insert strC {:id id :s s}))

;; Function to insert an idC into the table
(fn create-idC [id i]
  (table.insert idC {:id id :i i}))

;; Function to insert an idC into the table
(fn create-appC [id funID argsIDs]
  (table.insert appC {:id id :funID funID :argsIDs argsIDs}))

;; Function to insert a lamC into the table
(fn create-lamC [id argsIDs body]
  (table.insert lamC {:id id :argsIDs argsIDs :body body}))

;; Function to insert an ifC into the table
(fn create-ifC [id ifID thenID elseID]
  (table.insert ifC {:id id :ifID ifID :thenID thenID :elseID elseID}))

;; Retrieve functions
;; Function to retrieve stored val in numC
(fn get-numC [id]
  (var ans nil)
  (each [_ row (ipairs numC)]
    (when (= row.id id)
      (set ans row.n)))
  ans) 

;; function to retrieve stored val in strC
(fn get-strC [id]
  (var ans nil)
  (each [_ row (ipairs strC)]
    (when (= row.id id)
      (set ans row.s)))
  ans)

;; function to retrieve stored val in idC
(fn get-idC [id]
  (var ans nil)
  (each [_ row (ipairs idC)]
    (when (= row.id id)
      (set ans row.i)))
  ans)

;; function to retrieve appC row
(fn get-appC [id]
  (var ans nil)
  (each [_ row (ipairs appC)]
    (when (= row.id id)
      (set ans row)))
  ans)

;; function to retrieve lamC row
(fn get-lamC [id]
  (var ans nil)
  (each [_ row (ipairs lamC)]
    (when (= row.id id)
      (set ans row)))
  ans)

;; function to retrieve lamC row
(fn get-ifC [id]
  (var ans nil)
  (each [_ row (ipairs ifC)]
    (when (= row.id id)
      (set ans row)))
  ans)


;; Insert values
(create-numC "myNum" 42)
(create-strC "myStr" "I hate fennel")
(create-idC "myID" "x") ; we have to store ids as strings becasue in Fennel symbols are interned and this would break scope
(create-appC "myAppC" "myFunID" ["arg1ID" "arg2ID" "arg3ID"])
(create-lamC "myLamC" ["arg1" "arg2"] "bodyID")
(create-ifC "myIfC" "true" 12 10)

;; Print stored values
(print (get-numC "myNum"))
(print (get-strC "myStr"))
(print (get-idC "myID"))
(var myApp (get-appC "myAppC"))
(each [key value (pairs myApp)] ;;freaky stuff to get it to print properly
  (if (= (type value) "table")
    (each [inKey inValue (pairs value)]
      (io.write inValue " "))
    (print value)))
(print "")
(var myLam (get-lamC "myLamC"))
(each [key value (pairs myLam)] ;;freaky stuff to get it to print properly (again)
  (if (= (type value) "table")
    (each [inKey inValue (pairs value)]
      (io.write inValue " "))
    (print value)))
(print "")
(var myIfC (get-ifC "myIfC"))
(each [key value (pairs myIfC)]
  (print value))