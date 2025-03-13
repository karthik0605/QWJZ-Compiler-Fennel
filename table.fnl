;; insert functions 

;;ids have to be strings, not symbols

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
  {:type "cloV" :args args :body body :env clo-env})

(fn create-primopV [op]
  {:type "primopV" :op op})

(fn create-bind [id val]
  {:type "bind" :id id :val val})

;; parser (time permitting)
(local top-env [(create-bind (create-idC :+) (create-primopV :+))
                (create-bind (create-idC :-) (create-primopV :-))
                (create-bind (create-idC :*) (create-primopV :*))
                (create-bind (create-idC :/) (create-primopV :/))])

(fn serialize [val]
  (match val
    {:type "numV" :n n} n
    {:type "strV" :str str} str
    {:type "boolV" :b b} b))

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

(fn shallow-copy [tbl]
  (do
    (var new-tbl {})
    (each [k v (pairs tbl)] 
      (tset new-tbl k v))  
    new-tbl))

(fn extend-closure [args-id args-value clo-env]
  (if (= (length args-id) (length args-value))
      (do
        (var new-env (shallow-copy clo-env))
        (for [i 1 (length args-id)]
          (do
            (var bind (create-bind (. args-id i) (. args-value i)))
            (table.insert new-env bind)))
        new-env)
      (error "QWJZ: Number of variables and arguments don't match")))

(fn lookup [id env] 
  (var ans nil)
  (for [i (length env) 1 -1]
    (do
      (local item (. env i))
      (if (= id.id item.id.id)
        (set ans item.val)))) ans)

;; Interp function
(fn interp [ast interp-env]
  (match ast
    {:type "numC" :n n} (create-numV n)
    {:type "strC" :str str} (create-strV str)
    {:type "idC" :id id} (lookup (create-idC id) interp-env)
    {:type "appC" :funID funID :argsIDs argsIDs}
    (do
        (local fun (interp funID interp-env))
        (match fun
            {:type "primopV" :op op} 
            (do
              (local interped-args [])
              (for [i 1 (length argsIDs)]
                (table.insert interped-args (interp (. argsIDs i) interp-env)))
              (apply-op op interped-args))
            {:type "cloV" :args args :body body :env clo-env}
            (do
              (local interped-args [])
              (for [i 1 (length argsIDs)]
                (table.insert interped-args (interp (. argsIDs i) interp-env)))
              (interp body (extend-closure args interped-args clo-env)))
            _ (print "error")))
    {:type "lamC" :argsIDs argIDs :body body} (create-cloV argIDs body interp-env)
    {:type "ifC" :ifID ifID :thenID thenID :elseID elseID}
    (do
        (local ans (interp ifID interp-env))
        (match ans
            {:type "boolV" :b b} 
            (if b
                (interp thenID interp-env)
                (interp elseID interp-env))
            _ "if no work"))        
    _ (error "Cheeseballs")))  ;; Catch any unknown ASTs


;; Test cases
(assert (= (serialize (interp (create-numC 5) top-env)) 5))
(assert (= (serialize (interp (create-strC "I hate fennel") top-env)) "I hate fennel"))
(assert (= (serialize (interp (create-appC (create-idC :+) [(create-numC 5) (create-numC 6)]) top-env)) 11))
(assert (= (serialize (interp 
(create-appC (create-lamC [(create-idC :x)] (create-appC (create-idC :+) 
[(create-idC :x) (create-numC 1)])) [(create-numC 3)]) top-env)) 4))