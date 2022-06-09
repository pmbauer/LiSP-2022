(ns codes.bauer.LiSP.ch01
  "Lisp in Small Pieces Ch1"
  {:clj-kondo/config '{:linters {:unresolved-symbol {:level :off}}}}
  (:require [hyperfiddle.rcf :refer [tests]]))

(def literal? (some-fn number? string? char? boolean? vector?))

(defn atom? [exp]
  (or (literal? exp) (symbol? exp)))

(defn wrong [& msg]
  (throw (RuntimeException. (apply str msg))))

(defn lookup [env sym]
  (let [val (get-in env [::locals sym] ::notfound)]
    (if (= ::notfound val)
      (let [val (get env sym ::notfound)]
        (if (= ::notfound val)
          (wrong "No such binding " sym)
          val))
      val)))

(defn update-env [env sym val]
  (let [localpath [::locals sym]
        localval (get-in env localpath ::notfound)
        path (if (= ::notfound localval)
               (rest localpath)
               localpath)]
    (assoc-in env path val)))

(defn ^:dynamic choose-binding [lexical _]
  lexical)

(defn invocation-env [defenv callenv params vals]
  (if (= (count params) (count vals))
    (assoc callenv ::locals (merge (::locals (choose-binding defenv callenv))
                                   (zipmap params vals)))
    (wrong "incorrect number of args")))

(declare eprogn)
(declare evseq)

(defn make-function
  "[definition-env params body] -> [env argvals] -> [env' result]"
  [defenv params body]
  (fn [env vals]
    (let [caller-locals (::locals env)
          env-with-locals (invocation-env defenv env params vals)
          [env' res] (eprogn env-with-locals body)]
      [(assoc env' ::locals caller-locals) res])))

(defn invoke [env f args]
  (if (fn? f)
    (f env args)
    ;; exercise 1.1 tracing
    #_(let [[env' res :as rv] (f env args)]
      (println "Called (" f " " args ") => " res)
      rv)
    (wrong "not a function " f)))

(defn evaluate
  "returns [env' val]"
  [env exp]
  (if (atom? exp)
    (cond (symbol? exp) [env (lookup env exp)]
          (literal? exp) [env exp]
          :else (wrong "Cannot evaluate " exp))
    (let [[car & [cadr caddr cadddr :as cdr]] exp]
      (condp = car
        'quote [env cadr]
        'if (let [[env' res] (evaluate env cadr)]
              (if (not= ::false res)
                (recur env' caddr)
                (recur env' cadddr)))
        'set! (let [[env' val] (evaluate env caddr)]
                [(update-env env' cadr val) val])
        'begin (eprogn env cdr)
        'lambda [env (make-function env cadr (rest cdr))]
        (let [[env' f] (evaluate env car)
              [env'' args] (evseq env' cdr)]
          (invoke env'' f args))))))

(defn evreduce [env f val s]
  (reduce (fn [[env col] exp]
            (let [[env' res] (evaluate env exp)]
              [env' (f col res)]))
          [env val]
          s))

(defn eprogn
  "[env exp-seq] -> [env' last-result]"
  [env s]
  (evreduce env (comp second list) nil s))

(defn evseq
  "[env exp-seq] -> [env' result-col]"
  [env s]
  (evreduce env conj [] s))

(defmacro definitial [env name value]
  `(update-env ~env (quote ~name) ~value))

(defmacro lift
  "takes a native function and lifts it into our baby scheme; defprimitive is an imprecise name"
  [env name f arity]
  `(definitial ~env ~name
     (fn [env# values#]
       (if (= ~arity (count values#))
         [env# (apply ~f values#)]
         (wrong "Incorrect arity " '~name " " values#)))))

(defmacro lift-predicate [env name f arity]
  `(lift ~env ~name #(or (apply ~f %&) ::false) ~arity))

(def baby
  "baby scheme"
  (-> {}
      (definitial |t| true)
      (definitial |f| ::false)
      (lift cons cons 2)
      (lift car first 1)
      (lift cdr rest 1)
      (lift + + 2)
      (lift - - 2)
      (lift * * 2)
      (lift remainder rem 2)
      (lift quotient quot 2)
      (lift display #(print %) 1)
      (lift newline #(println) 0)
      (definitial list
        (fn [env vals]
          [env (apply list vals)]))
      (definitial apply
        (fn [env [f & args]]
          (if (>= (count args) 1)
            (invoke env f (apply list* args))
            (wrong "Incorrect arity " 'apply ": " args))))
      (lift-predicate <= <= 2)
      (lift-predicate >= >= 2)
      (lift-predicate = = 2)
      (lift-predicate > > 2)
      (lift-predicate < < 2)
      (lift-predicate symbol? symbol? 1)
      (lift-predicate eq? = 2)))

(defn a-corner [thing]
  (when (= baby thing)
    (wrong "Nobody puts baby in a corner.")))

(tests
  "vars and literals"
  (evaluate '{a 1} 'a) := [_ 1]
  (evaluate '{a 1} 4) := [_ 4]
  (evaluate '{a 1} \c) := [_ \c]

  "quoting"
  (evaluate '{a 1} '(quote a)) := [_ 'a]

  "set!"
  (evaluate '{a 1} '(set! b a)) := '[{a 1, b 1} 1]

  "side effects and closures"
  (-> (evaluate '{a 0}
                '(((lambda (c)
                    (set! e c)     ;; captures local c
                    (set! c 4)     ;; updates local c
                    (lambda (a)    ;; shadows global a
                      (set! d a)   ;; captures local a
                      (set! a c)   ;; writes to local a
                      (set! b a))) ;; captures closed over c value, final return
                   2)
                  3))) := ['{a 0, b 4, d 3, e 2, ::locals nil} 4]

  "booleans and branches"
  (evaluate baby '|f|) := [_ ::false]
  (evaluate baby '|t|) := [_ true]
  (-> baby
      (evaluate '(if (< 0 1)
                   (set! x 1)
                   (wrong "question everything")))
      first
      (lookup 'x)) := 1

  "lists"
  (evaluate baby '(car (quote (1 2)))) := [_ 1]
  (evaluate baby '(cdr (quote (1 2)))) := [_ '(2)]
  (evaluate baby '(cons 0 (quote (1 2)))) := [_ '(0 1 2)]
  (evaluate baby '(list 1 2 3 4)) := [_ '(1 2 3 4)]

  "apply"
  (evaluate baby '(apply + 2 (list 2))) := [_ 4]
  (evaluate baby '(apply * (list 5 5))) := [_ 25]

  "dynamic binding"
  (def foo-bar-example ;; taken from section 1.6.1
    '(begin
       (set! foo (lambda (x) (list x y)))
       (set! bar (lambda (y) (foo 1991)))
       (set! y 0)
       (list (bar 100) (foo 3))))
  (evaluate baby foo-bar-example) := [_ '((1991 0) (3 0))]    ;; lexical binding
  (binding [choose-binding (fn [_ dynamic] dynamic)]
    (evaluate baby foo-bar-example)) := [_ '((1991 100) (3 0))]

  "recursion"
  (-> baby
      (evaluate '(begin
                   (set! fib
                         (lambda (n)
                           (if (< n 2)
                             n
                             (+ (fib (- n 1)) (fib (- n 2))))))
                   (fib 8)))
      second) := 21)
