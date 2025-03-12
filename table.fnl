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

;; Interp function
(fn interp [ast]
  (match ast
    {:type "numC" :n n} n
    {:type "strC" :str str} str  
    {:type "idC" :id id} id 
    {:type "appC" :funID funID :argsIDs argsIDs}
    (do
        (local fun (interp funID))
        (match fun
            {:type "primopV" :op op} (apply-op op argsIDs)
            {:type "cloV" :args args :body body :clo-env clo-env}
            (interp body (extend-closure args (interp-args argsIDs env) clo-env))
            ))
    {:type "lamC" :argIDs argIDs :body body} (create-cloV argIds body env)
    {:type "ifC" :ifID ifID :thenID thenID :elseID elseID}
    (do
        (local ans (interp ifID env))
        (match ans
            {:type "boolV" :b b} 
            (if b
                (interp thenID env)
                (interp elseID env))
            _ "if no work"))        
    _ (error "Cheeseballs" ast)))  ;; Catch any unknown ASTs

(fn apply-op [op args]
    (match op
        :+ (create-numV (+ (first args.n) (second args.n)))
        :- (create-numV (- (first args.n) (second args.n)))
        :* (create-numV (* (first args.n) (second args.n)))
        :/ (create-numV (/ (first args.n) (second args.n)))
        _ (error "terrible operators" op)))

;;have clo-env and stuff locally scoped
(fn extend-closure [args-id args-value clo-env]
  (if (= (length args-id) (length args-value))
      (append (create-bindings args-id args-value) clo-env)
      (error 'interp "QWJZ: Number of variables and arguments don't match")))

;;Helper function to create bindings for each id and value (as a list of bindings)
(fn create-bindings [args-id args-value]
  (match (list args-id args-value)
    [] [] 
    [first-id rest-id] [(create-bind first-id (first args-value))].insert(create-bindings rest-id (rest args-value))))


;; Test cases
(print (interp (create-numC 5)))  ;; Should print 5
(print (interp (create-strC "I hate fennel")))  ;; Should print "I hate fennel"
(print (interp (create-idC :y)))  ;; Should print :y (the id itself)