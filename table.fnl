;; insert functions
(fn create-numC [n]
  {:type "numC" :n n})  ;; Adding type "num"

(fn create-strC [str]
  {:type "strC" :str str})  ;; Adding type "str"

(fn create-idC [id]
  {:type "idC" :id id})  ;; Adding type "id"

(fn create-appC [funID argsIDs]
  {:type "appC" :funID funID :argsIDs argsIDs})

(fn create-lamC [argsIDs body]
  {:type "lamC" :argsIDs argsIDs :body body})

(fn create-ifC [ifID thenID elseID]
  {:type "ifC" :ifID ifID :thenID thenID :elseID elseID})

(fn create-numV [n]
  {:type "numV" :n n})  

(fn create-strV [str]
  {:type "strV" :str str})  

(fn create-boolV [b]
  {:type "boolV" :b b})  

(fn create-cloV [args body clo-env]
  {:type "cloV" :args args :body body :clo-env clo-env})

(fn create-primopV [op]
  {:type "primopV" :op op})

(fn create-bind [id val]
  {:type "bind" :id id :val val})

;; parser (time permitting)
(local top-env [(create-bind :+ (create-primopV :+))
                (create-bind :- (create-primopV :-))
                (create-bind :* (create-primopV :*))
                (create-bind :/ (create-primopV :/))])

(fn serialize [val]
  (match val
    {:type "numV" :n n} n
    {:type "boolV" :b b} b
    {:type "strV" :s s} s))

(fn apply-op [op args]
    ;if statement for length of args
    (do
      (local arg1 (. args 1))
      (local arg2 (. args 2))
      (match op
          :+ (create-numV (+ arg1.n arg2.n))
          :- (create-numV (- arg1.n arg2.n))
          :* (create-numV (* arg1.n arg2.n))
          :/ (create-numV (/ arg1.n arg2.n))
          _ (error "terrible operators" op))))

(fn lookup [id env] 
  (var ans nil)
  (for [i 1 (length env)]
    (do
      (local item (. env i))
      (if (= id item.id)
            (set ans item.val)))) ans)
            
;; Interp function
(fn interp [ast top-env]
  (match ast
    {:type "numC" :n n} (create-numV n)
    {:type "strC" :str str} (create-strV str)
    {:type "idC" :id id} (lookup id top-env)
    {:type "appC" :funID funID :argsIDs argsIDs}
    (do
        (local fun (interp funID top-env))
        (match fun
            {:type "primopV" :op op} (apply-op op argsIDs)
            ; {:type "cloV" :args args :body body :clo-env clo-env}
            ; (interp body (extend-closure args (interp-args argsIDs env) clo-env))
            ))
    {:type "lamC" :argIDs argIDs :body body} (create-cloV argIDs body top-env)
    ; {:type "ifC" :ifID ifID :thenID thenID :elseID elseID}
    ; (do
    ;     (local ans (interp ifID env))
    ;     (match ans
    ;         {:type "boolV" :b b} 
    ;         (if b
    ;             (interp thenID env)
    ;             (interp elseID env))
    ;         _ "if no work"))        
    _ (error "Cheeseballs" ast)))  ;; Catch any unknown ASTs


;;have clo-env and stuff locally scoped
;fennel specific syntax
; (fn extend-closure [args-id args-value clo-env]
;   (if (= (length args-id) (length args-value))
;       (append (create-bindings args-id args-value) clo-env)
;       (error 'interp "QWJZ: Number of variables and arguments don't match")))

; ;;Helper function to create bindings for each id and value (as a list of bindings)
; (fn create-bindings [args-id args-value]
;   (match (list args-id args-value)
;     [] [] 
;     [first-id rest-id] [(create-bind first-id (first args-value))].insert(create-bindings rest-id (rest args-value))))


;; Test cases
;(print (serialize (interp (create-numC 5) top-env)))  ;; Should print 5
;(print (serialize (interp (create-strC "I hate fennel") top-env)))  ;; Should print "I hate fennel"
;(print (interp (create-idC :y)))
(print (serialize (interp (create-appC (create-idC :+) [(create-numC 5) (create-numC 6)]) top-env)))