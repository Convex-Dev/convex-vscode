(ns convex-vscode.core
  (:require  [clojure.string :as str]
             [cljfmt.core :as cljfmt]
             ["vscode" :as vscode]))

(def convex-core-reference
  "Convex core indexed by symbol."

  '{"NaN" {:description "Not-a-Number double value, as defined by IEEE 754, and implemented on the JVM"
           :signature nil
           :symbol "NaN"
           :examples [{:code "(double? NaN)"}]
           :type :value}
    "*depth*" {:description "Returns the current CVM stack depth. If the depth becomes too deep, the transaction will fail with a DEPTH exception. In most cases, the allowable depth should be sufficient."
               :signature nil
               :symbol "*depth*"
               :examples [{:code "*depth*"}]
               :type :special}
    "boolean" {:description "Casts any value to a Boolean. Returns true if the value is truthy, false otherwise."
               :signature [{:params [a]}]
               :symbol "boolean"
               :examples [{:code "(boolean 123)"}]
               :type :function}
    "floor" {:description "Computes the mathematical floor (rounding down towards negative infinity) for a numerical argument. Uses double precision mathematics."
             :signature [{:params [x]}]
             :symbol "floor"
             :examples [{:code "(floor 16.3)"}]
             :type :function}
    "map" {:description "Applies a function to each element of a data structure in sequence, and returns a vector of results. Additional collection may be provided to call a function with higher arity."
           :signature [{:params [f coll]} {:params [f coll1 coll2 & more-colls]}]
           :symbol "map"
           :examples [{:code "(map inc [1 2 3])"}]
           :type :function}
    "ceil" {:description "Computes the mathematical ceiling (rounding up towards positive infinity) for a numerical argument. Uses double precision mathematics."
            :signature [{:params [x]}]
            :symbol "ceil"
            :examples [{:code "(floor 16.3)"}]
            :type :function}
    "call*" {:description "Calls an Actor function. address must cast to Address. offer must cast to Long. (symbol fn-name) must identify an exported function in the target Actor. args must be valid arguments for the called function."
             :signature [{:params [address offer fn-name & args]}]
             :symbol "call*"
             :examples [{:code "(call* some-actor 1000 'actor-fn arg1 arg2)"}]
             :type :function}
    "deploy" {:description "Deploys an actor. The code provided will be executed to initialise the Actor's account."
              :signature [{:params [code]}]
              :symbol "deploy"
              :examples [{:code "(deploy '(do (defn my-fn [x y] (+ x y)) (export my-fn)) )"}]
              :type :function}
    "fetch" {:description "Fetches the object with the given Object ID from immutable storage. Returns nil if the value is not found. Object ID may be a Hash or a Blob of correct length."
             :signature [{:params [id]}]
             :symbol "fetch"
             :examples [{:code "(fetch object-id)"}]
             :type :function}
    "vec" {:description "Coerces the argument to a Vector. Arguement must be coercible to a sequence."
           :signature [{:params [coll]}]
           :symbol "vec"
           :examples [{:code "(vec #{1 2 3 4})"}]
           :type :function}
    "values" {:description "Gets the values from a map data structure."
              :signature [{:params [m]}]
              :symbol "values"
              :examples [{:code "(values {1 2 3 4})"}]
              :type :function}
    "loop" {:description "Creates a loop body, binding one or more loop variables in a manner similar to 'let'. Within the loop body, 'recur' can be used to return to the start of the loop while re-binding the loop variables with new values. Does not consume stack."
            :signature [{:params [bindings & body]}]
            :symbol "loop"
            :examples [{:code "(loop [i 10 acc 1] (if (> i 1) (recur (dec i) (* acc i)) acc))"}]
            :type :special}
    "min" {:description "Returns the numerical minimum of the given values."
           :signature [{:params [fst & vals]}]
           :symbol "min"
           :examples [{:code "(min 1 2 3)"}]
           :type :function}
    "long?" {:description "Tests if the argument is a Long value."
             :signature [{:params [a]}]
             :symbol "long?"
             :examples [{:code "(long? 1234)"}]
             :type :function}
    "balance" {:description "Returns the coin balance of the specified account, which must cast to Address."
               :signature [{:params [address]}]
               :symbol "balance"
               :examples [{:code "(balance *caller*)"}]
               :type :function}
    "defn" {:description "Defines a function in the current environment."
            :signature [{:params [name params & body]}]
            :symbol "defn"
            :examples [{:code "(defn my-square [x] (* x x))"}]
            :type :macro}
    "defactor" {:description "Defines an actor generation function in the environment. See 'actor' for more details on actor generation functions."
                :signature [{:params [name params & body]}]
                :symbol "defactor"
                :examples [{:code "(defactor multiply-actor [x] (defn calc [y] (* x y)) (export calc))"}]
                :type :macro}
    "=" {:description "Tests if arguments are equal in value."
         :signature [{:params [& vals]}]
         :symbol "="
         :examples [{:code "(= :foo :foo)"}]
         :type :function}
    "list" {:description "Creates a List containing the given arguments as elements."
            :signature [{:params [& elements]}]
            :symbol "list"
            :examples [{:code "(list 1 2 3)"}]
            :type :function}
    "syntax?" {:description "Tests if the argument is a Syntax Object."
               :signature [{:params [a]}]
               :symbol "syntax?"
               :examples [{:code "(syntax? form)"}]
               :type :function}
    "*" {:description "Multiplies numeric arguments. Result will be a Double if any arguments are Doubles, otherwise it will be a Long."
         :signature [{:params [& xs]}]
         :symbol "*"
         :examples [{:code "(* 1 2 3 4 5)"}]
         :type :function}
    "next" {:description "Returns the elements of a sequential data structure after the first, or null if no more elements remain."
            :signature [{:params [coll]}]
            :symbol "next"
            :examples [{:code "(next [1 2 3])"}]
            :type :function}
    "keys" {:description "Returns a vector of keys in the given map, in the map defined order."
            :signature [{:params [m]}]
            :symbol "keys"
            :examples [{:code "(keys {:foo 1 :bar 2})"}]
            :type :function}
    "*timestamp*" {:description "Returns the current timestamp. The timestamp is a Long value that is equal to the greatest timestamp of any block executed (including the current block). A timestamp can be interpreted as the number of milliseconds since January 1, 1970, 00:00:00 GMT. The block timestamp should always be less than or equal to the Unix timestamp of Peers that are in consensus."
                   :signature nil
                   :symbol "*timestamp*"
                   :examples [{:code "*timestamp*"}]
                   :type :special}
    "call" {:description "Calls an function in another Account. The specified address must be valid, and refer to an existing account. offer is optional, if provided must cast to Long. call-form must be a valid expression calling an exported function in the target Account."
            :signature [{:params [address call-form]} {:params [address offer call-form]}]
            :symbol "call"
            :examples [{:code "(call some-contract 1000 (contract-fn arg1 arg2))"}]
            :type :macro}
    "<=" {:description "Tests if numeric arguments are in increasing order. Reads as 'less-than-or-equal'."
          :signature [{:params [& xs]}]
          :symbol "<="
          :examples [{:code "(<= 1 1 3)"}]
          :type :function}
    "number?" {:description "Returns true if the argument is a numeric value, false otherwise."
               :signature [{:params [a]}]
               :symbol "number?"
               :examples [{:code "(number? 2.3)"}]
               :type :function}
    "dissoc" {:description "Removes entries with the specified key(s) from a map. Returns the same map unchanged if the key is not present."
              :signature [{:params [coll & keys]}]
              :symbol "dissoc"
              :examples [{:code "(dissoc {1 2 3 4} 3)"}]
              :type :function}
    "vector?" {:description "Tests if the argument is a Vector."
               :signature [{:params [a]}]
               :symbol "vector?"
               :examples [{:code "(vector? [1 2 3])"}]
               :type :function}
    "reduced" {:description "Returns immediately from the enclosing 'reduce' function, providing the given value as the result of the whole reduce operation. This can be used to terminate early from a reduce operation, saving transaction costs."
               :signature [{:params [result]}]
               :symbol "reduced"
               :examples [{:code "(reduce (fn [acc x] (reduced :exit)) 1 [1 2 3 4 5])"}]
               :type :function}
    "not" {:description "Inverts a truth value. Returns false for truthy input and true for falsey input."
           :signature [{:params [b]}]
           :symbol "not"
           :examples [{:code "(not true)"} {:code "(not nil)"}]
           :type :function}
    "expand" {:description "Expands the given form, including expansion of any macros. Uses the specified expander if provided, *initial-expander* otherwise."
              :signature [{:params [form]} {:params [form expander]}]
              :symbol "expand"
              :examples [{:code "(expand '(if a :truthy :falsey))"}]
              :type :function}
    "if-let" {:description "Binds a symbol to the result of evaluating a given expression, and executes a true expression or false expression depending on the result in the same manner as 'if'"
              :signature [{:params [[sym exp] & body]}]
              :symbol "if-let"
              :examples [{:code "(when-let [addr  (get-address)] (transfer addr 1000))"}]
              :type :macro}
    "export" {:description "Exports symbols from the current actor. Exported symbols may be called by external parties. It is advised to carefully audit the security of usages of this function: normally, an actor's deployment code should have a single export statement that exports functions that have been validated as safe and intended to provide a public API to the actor."
              :signature [{:params [& symbols]}]
              :symbol "export"
              :examples [{:code "(export some-safe-function)"}]
              :type :macro}
    "blob" {:description "Casts the argument to a Blob. Handles Addresses, Hashes, existing blobs, and hex Strings"
            :signature [{:params [address]}]
            :symbol "blob"
            :examples [{:code "(blob \"1234abcd\")"}]
            :type :function}
    "hash-map" {:description "Constructs a hash map with the given keys and values. If a key is repeated, the last value will overwrite previous ones."
                :signature [{:params [& kvs]}]
                :symbol "hash-map"
                :examples [{:code "(hash-map 1 2 3 4)"}]
                :type :function}
    "halt" {:description "Completes execution in the current context with the specified result, or null if not provided. Does not roll back any state changes made. If the currently executing context is an actor, the result will be used as the return value from the actor call."
            :signature [{:params []} {:params [result]}]
            :symbol "halt"
            :examples [{:code "(halt :we-are-finished-here)"}]
            :type :function}
    "/" {:description "Double precision point divide. With a single argument, returns the reciprocal of a number. With multiple arguments, divides the first argument by the others in order."
         :signature [{:params [divisor]} {:params [numerator divisor]} {:params [numerator divisor & more]}]
         :symbol "/"
         :examples [{:code "(/ 10 3)"}]
         :type :function}
    "str" {:description "Coerces values to Strings and concatenates them."
           :signature [{:params [& args]}]
           :symbol "str"
           :examples [{:code "(str \"Hello \" name)"}]
           :type :function}
    "long" {:description "Casts the given argument to a 64-bit signed Long."
            :signature [{:params [num]}]
            :symbol "long"
            :examples [{:code "(long 10)"}]
            :type :function}
    "hash" {:description "Calculates the 32-byte SHA3-256 cryptographic Hash of a Blob. Also works with Hash and Address arguments (since these are specialised types of Blob)."
            :signature [{:params [value]}]
            :symbol "hash"
            :examples [{:code "(hash 0x1234)"} {:code "(hash (encoding :foo))"}]
            :type :function}
    "return" {:description "Escapes from the currently executing code and returns the specified value from the current function. Expressions following the 'return' will not be executed."
              :signature [{:params [value]}]
              :symbol "return"
              :examples [{:code "(return :finished)"}]
              :type :special}
    "-" {:description "Subtracts numerical arguments from the first argument. Negates a single argument."
         :signature [{:params [x]} {:params [x y & more]}]
         :symbol "-"
         :examples [{:code "(- 10 7)"}]
         :type :function}
    "*balance*" {:description "Returns the available balance of the current account. Functionally equivalent to '(balance *address*)', but cheaper to execute. Note that the available balance excludes reserved balance for transaction execution, so this number may be somewhat less than the total account balance during transaction execution."
                 :signature nil
                 :symbol "*balance*"
                 :examples [{:code "*balance*"}]
                 :type :special}
    "query" {:description "Runs forms in query mode. Results will be retained, but any state changes will be rolled back."
             :signature [{:params [& forms]}]
             :symbol "query"
             :examples [{:code "(query (def a 10) a)"} {:code "(query (call unsafe-actor (do-something)))"}]
             :type :special}
    "contains-key?" {:description "Tests if the given asccoiative data structure contains the given key."
                     :signature [{:params [coll key]}]
                     :symbol "contains-key?"
                     :examples [{:code "(contains-key? {:foo 1 :bar 2} :foo)"}]
                     :type :function}
    "second" {:description "Retuens the second element of a sequential collection."
              :signature [{:params [coll]}]
              :symbol "second"
              :examples [{:code "(second [1 2 3])"}]
              :type :function}
    "unsyntax" {:description "Unwraps a value from a syntax object. If the argument is not a Syntax Object, returns it unchanged."
                :signature [{:params [form]}]
                :symbol "unsyntax"
                :examples [{:code "(unsyntax form)"}]
                :type :function}
    "union" {:description "Computes the union of zero or more sets. 'nil' is treated as the empty set."
             :signature [{:params [& sets]}]
             :symbol "union"
             :examples [{:code "(union #{1 2} #{2 3})"}]
             :type :function}
    "max" {:description "Returns the numerical maximum of the given values."
           :signature [{:params [fst & vals]}]
           :symbol "max"
           :examples [{:code "(max 1 2 3)"}]
           :type :function}
    "get-in" {:description "Gets an element by sucessively looking up keys in a collection according to the logic of get. If any lookup does not find the appropriate key, will return nil (or the not-found value if provided)."
              :signature [{:params [coll keys]} {:params [coll keys not-found]}]
              :symbol "get-in"
              :examples [{:code "(get-in [[1 2] [3 4]] [1 1])"}]
              :type :function}
    "stake" {:description "Sets the stake on a given peer. Peer must exist, and funds must be available to set the stake to the specified level."
             :signature [{:params [address amount]}]
             :symbol "stake"
             :examples [{:code "(stake trusted-peer-address 7000000000)"}]
             :type :function}
    "nth" {:description "Gets the nth element of a sequential collection. Collection must be coercible to a sequence, and the index must be a valid long between 0 (inclusive) and the count of the collection (exclusive)."
           :signature [{:params [coll index]}]
           :symbol "nth"
           :examples [{:code "(nth [1 2 3] 2)"}]
           :type :function}
    "transfer-memory" {:description nil, :signature nil, :symbol "transfer-memory", :examples nil}
    "encoding" {:description "Returns the byte encoding for a given value, as a Blob. The encoding is the unique canonical binary representation of a value. Encodings may change between Convex versions - it is unwise to rely on the exact representation."
                :signature [{:params [value]}]
                :symbol "encoding"
                :examples [{:code "(encoding {1 2})"}]
                :type :function}
    "set!" {:description "Sets a local binding identified by an unqualified symbol to the given value. This local change will be be visible until the scope leaves the current binding form (e.g. let binding, function body or recur). Fails with an ARGUMENT error if the symbol is qualified. This is probably most useful for updating a local variable in imperative style. Returns the value assigned to the local binding if successful."
            :signature [{:params [sym value]}]
            :symbol "set!"
            :examples [{:code "(let [a 10] (set! a 20) a)"}]
            :type :macro}
    "fn" {:description "Creates an anonymous function (closure) with the specified argument list and function body. Will close over variables in the current lexical scope."
          :signature [{:params [args & body]}]
          :symbol "fn"
          :examples [{:code "(let [f (fn [x y] (* x y))] (f 10 7))"}]
          :type :special}
    "pow" {:description "Returns the first argument raised to the power of the second argument. Uses double precision maths."
           :signature [{:params [x y]}]
           :symbol "pow"
           :examples [{:code "(pow 2 3)"}]
           :type :function}
    "for" {:description "Executes the body with the symbol 'sym' bound to each value of the given sequence. Returns a vector of results."
           :signature [{:params [[sym sequence] & body]}]
           :symbol "for"
           :examples [{:code "(for [x [1 2 3]] (inc x))"}]
           :type :macro}
    "lookup" {:description "Looks up the value of a symbol in the current execution environment, or the account of the given address if specified. Name may be a symbol, keyword or String. Returns nil if not found."
              :signature [{:params [name]} {:params [address name]}]
              :symbol "lookup"
              :examples [{:code "(lookup 'count)"}]
              :type :function}
    "count" {:description "Counts the number of elements in the given collection. Returns the length of Blobs and Strings."
             :signature [{:params [coll]}]
             :symbol "count"
             :examples [{:code "(count [1 2 3])"}]
             :type :function}
    "empty?" {:description "Checks if the argument is an empty collection. nil is considered empty. "
              :signature [{:params [coll]}]
              :symbol "empty?"
              :examples [{:code "(empty? [])"}]
              :type :function}
    "cons" {:description "Constructs a List, by prepending the leading arguments to the last argument. The last argument must be coercable to a sequence."
            :signature [{:params [& args]}]
            :symbol "cons"
            :examples [{:code "(cons 1 '(2 3))"} {:code "(cons 1 2 '(3 4))"}]
            :type :function}
    "log" {:description nil, :signature nil, :symbol "log", :examples nil}
    "apply" {:description "Applies a function to the specified arguments, after flattening the last argument. Last argument must be a sequential collection, or 'nil' which is considered and empty collection."
             :signature [{:params [f & more-args]}]
             :symbol "apply"
             :examples [{:code "(apply + [1 2 3])"} {:code "(apply + 1 2 [3 4 5])"}]
             :type :function}
    "account?" {:description "Tests if the given address refers to a valid existing acount (which may be either an Actor or User account)."
                :signature [{:params [address]}]
                :symbol "account?"
                :examples [{:code "(account? *caller*)"}]
                :type :fn}
    "actor?" {:description "Tests if the given Address refers to an Actor. Argument must be castable to Address"
              :signature [{:params [address]}]
              :symbol "actor?"
              :examples [{:code "(actor? \"1Ba377262D7637068C8a84b732e30d3Ff62bA891\")"}]
              :type :function}
    "set-holding" {:description "Sets the holding value for a specified owner account address. Owner account must exist. Returns the new holding value."
                   :signature [{:params [owner value]}]
                   :symbol "set-holding"
                   :examples [{:code "(set-holding *caller* 1000)"}]
                   :type :function}
    "str?" {:description "Tests if the argument is a String."
            :signature [{:params [a]}]
            :symbol "str?"
            :examples [{:code "(str? \"foo\")"}]
            :type :function}
    "subset?" {:description "Tests if a set is a subset of a second set. Both arguments must be sets. nil is considered as an empty set."
               :signature [{:params [set1 set2]}]
               :symbol "subset?"
               :examples [{:code "(subset? #{1} #{1 2 3})"}]
               :type :function}
    "defexpander" {:description "Defines an expander in the current environment."
                   :signature [{:params [a]}]
                   :symbol "defexpander"
                   :examples [{:code "(defexpander expand-once [x e] (e x (fn [x e] x)))"}]
                   :type :macro}
    "name" {:description "Gets the String name of an object. Valid names can come from Strings, Keywords or Symbols."
            :signature [{:params [named-object]}]
            :symbol "name"
            :examples [{:code "(name :foo)"}]
            :type :function}
    "nil?" {:description "Tests if the argument is nil."
            :signature [{:params [a]}]
            :symbol "nil?"
            :examples [{:code "(nil? nil)"}]
            :type :function}
    "schedule" {:description "Schedules code for future execution under this account. Expands and compiles code now, but does not execute until the specified timestamp."
                :signature [{:params [timestamp code]}]
                :symbol "schedule"
                :examples [{:code "(schedule (+ *timestamp* 1000) (transfer my-friend 1000000))"}]
                :type :macro}
    "expander" {:description "Creates an expander from the given function."
                :signature [{:params [fn]}]
                :symbol "expander"
                :examples [{:code "(expander (fn [form ex] (quote form)))"}]
                :type :function}
    "create-account" {:description nil, :signature nil, :symbol "create-account", :examples nil}
    "cond" {:description "Performs conditional tests on successive (test, result) pairs of arguments, returning the result for the first test that succeeds. Performs short-circuit evaluation, i.e. result expressions that are not used and any test expressions after the first success will not be executed. In the case that no test succeeds, a single aditional argument may be added as a fallback value. If no fallback value is available, nil will be returned."
            :signature [{:params []}
                        {:params [test]}
                        {:params [test result]}
                        {:params [test result fallback-value]}
                        {:params [test1 result1 test2 result2 & more]}]
            :symbol "cond"
            :examples [{:code "(cond test1 result1 else-value)"}
                       {:code "(cond test1 result1 test2 result2 test3 result-3)"}]
            :type :special}
    "*exports*" {:description "A set of functions that are exported by the current smart contract. Exported functions may be called by any acount: suitable precautions should be applied."
                 :signature nil
                 :symbol "*exports*"
                 :examples [{:code "(assert (set? *exports*))"}]
                 :type :value}
    "*key*" {:description "Returns the public key for this Account. May be null (e.g. for an Actor)."
             :signature nil
             :symbol "*key*"
             :examples [{:code "*key*"}]
             :type :special}
    "if" {:description "Calculates the value of a test expression, then branches and returns the result of the true expression or false expression depending on whether the test expression is true or false respectively. The values 'false' and 'nil' are considered flase, all other values are true. Performs short-circuit evaluation, i.e. the expression in the unused branch is not evaluated. If the false branch is omitted, it will be assumed to evaluate to 'nil'. For a more general conditional expression that can handle multiple branches, consider 'cond' as an alternative."
          :signature [{:params [test true-exp]} {:params [test true-exp false-exp]}]
          :symbol "if"
          :examples [{:code "(if true :true-value :false-value)"}]
          :type :macro}
    "let" {:description "Binds local variables according to symbol / expression pairs in a binding vectors, then execute following expressions in an implicit do block."
           :signature [{:params [bindings & exps]}]
           :symbol "let"
           :examples [{:code "(let [x 10] (* x x))"}]
           :type :special}
    "import" {:description "Import a library for use in the current environment. Creates an alias to the library so that symbols defined in the library can be addressed directly in the form 'alias/symbol-name'. Returns the Address of the imported account."
              :signature [{:params [& args]}]
              :symbol "import"
              :examples [{:code "(import some.library :as alias)"}]
              :type :macro}
    "address?" {:description "Tests if the argument is an Address. Returns true if and only if the argument is an actual Address, not merely castable to one."
                :signature [{:params [a]}]
                :symbol "address?"
                :examples [{:code "(address? :foo)"}]
                :type :function}
    "inc" {:description "Increments the given number by 1. Converts to Long if necessary."
           :signature [{:params [num]}]
           :symbol "inc"
           :examples [{:code "(inc 10)"}]
           :type :function}
    "undef*" {:description nil, :signature nil, :symbol "undef*", :examples nil}
    "when-let" {:description "Executes the body with the symbol bound to the value of evaluating a given expression, if and only if the result of the expression is truthy. Returns nil otherwise."
                :signature [{:params [[sym exp] & body]}]
                :symbol "when-let"
                :examples [{:code "(when-let [addr  (get-address)] (transfer addr 1000))"}]
                :type :macro}
    "eval" {:description "Compiles and evaluates a form in the current context."
            :signature [{:params [form]}]
            :symbol "eval"
            :examples [{:code "(eval '(+ 1 2))"}]
            :type :function}
    "char" {:description "Casts a value to a Char. Discards high bits of larger integer types."
            :signature [{:params [a]}]
            :symbol "char"
            :examples [{:code "(char 97)"}]
            :type :function}
    "accept" {:description "Accepts offered coins up to the amount of *offer* from *caller*. Amount must cast to Long. If successful, the amount will be added immediately to the *balance* of the current *address*. This is the recommended way of transferring balance between Actors, as it requires a positive action to confirm receipt. Returns the amount accepted if successful. Returns a STATE error if there are insufficient funds to accept."
              :signature [{:params [amount]}]
              :symbol "accept"
              :examples [{:code "(accept *offer*)"}]
              :type :function}
    "address" {:description "Casts the argument to an Address. Valid arguments include hex Strings, Addresses and Blobs with the correct length (32 bytes)."
               :signature [{:params [a]}]
               :symbol "address"
               :examples [{:code "(address \"1Ba377262D7637068C8a84b732e30d3Ff62bA891\")"}]
               :type :function}
    "exp" {:description "Returns e raised to the power of the given numerical argument."
           :signature [{:params [x]}]
           :symbol "exp"
           :examples [{:code "(exp 1.0)"}]
           :type :function}
    "*result*" {:description "Returns the result of the last CVM operation executed. Can be used, in some cases, to access the value of the previous expression. Will be nil for new transactions, or at the start of an Actor call."
                :signature nil
                :symbol "*result*"
                :examples [{:code "(do 1 *result*)"}]
                :type :special}
    "vector" {:description "Creates a Vector with the given elements."
              :signature [{:params [& elements]}]
              :symbol "vector"
              :examples [{:code "(vector 1 2 3)"}]
              :type :function}
    "set-controller" {:description nil, :signature nil, :symbol "set-controller", :examples nil}
    "or" {:description "Executes expressions in sequence, returning the first truthy value, or the last value if all were falsey. Does not evaluate later expressions, so can be used to short circuit execution. Returns nil with no expressions present."
          :signature [{:params [& exprs]}]
          :symbol "or"
          :examples [{:code "(or nil 1)"}]
          :type :macro}
    "identity" {:description "An identity function which returns a single argument unchanged. Most useful when you want a 'do nothing' operation in higher order functions."
                :signature [{:params [a]}]
                :symbol "identity"
                :examples [{:code "(identity :foo)"} {:code "(map identity [1 2 3])"}]
                :type :function}
    "list?" {:description "Tests if the argument is a List data structure."
             :signature [{:params [a]}]
             :symbol "list?"
             :examples [{:code "(list? :foo)"}]
             :type :function}
    "*registry*" {:description "The address of the Convex registry Actor."
                  :signature nil
                  :symbol "*registry*"
                  :examples [{:code "(call *registry* (register {:name \"My name\"}))"}]
                  :type :value}
    "keyword?" {:description "Tests if the argument is a Keyword."
                :signature [{:params [a]}]
                :symbol "keyword?"
                :examples [{:code "(keyword? :foo)"}]
                :type :function}
    "keyword" {:description "Coerces the argument to a keyword."
               :signature [{:params [name]}]
               :symbol "keyword"
               :examples [{:code "(keyword \"foo\")"}]
               :type :function}
    "map?" {:description "Tests if an object is a map data structure."
            :signature [{:params [coll]}]
            :symbol "map?"
            :examples [{:code "(map? {1 2})"}]
            :type :function}
    "*offer*" {:description "Returns the amount of native coin offered by *caller* to the currently running Actor. Will usually be zero, unless the caller has included an offer with a 'call' expression. The Actor may take up to this amount using 'accept'."
               :signature nil
               :symbol "*offer*"
               :examples [{:code "*offer*"}]
               :type :special}
    "mapcat" {:description "Maps a funcion across the given collections, then concatenates the results. nil is treated as an empty collection."
              :signature [{:params [test & body]}]
              :symbol "mapcat"
              :examples [{:code "(mapcat vector [:foo :bar :baz] [1 2 3])"}]
              :type :function}
    ">=" {:description "Tests if numeric arguments are in decreasing order. Reads as 'greater-than-or-equal'."
          :signature [{:params [& xs]}]
          :symbol ">="
          :examples [{:code "(>= 3 2 2)"}]
          :type :function}
    "sqrt" {:description "Computes the square root of a numerical argument. Uses double precision mathematics. May return NaN for negative values."
            :signature [{:params [x]}]
            :symbol "sqrt"
            :examples [{:code "(sqrt 16.0)"}]
            :type :function}
    "concat" {:description "Concatenates sequential objects, returning a new sequential object of the same type as the first non-nil argument. Nil is treated as an empty sequence."
              :signature [{:params [& seqs]}]
              :symbol "concat"
              :examples [{:code "(concat [1 2] [3 4])"}]
              :type :function}
    "symbol" {:description "Coerces a named value to a Symbol. Supports Symbols, Keywords and short Strings."
              :signature [{:params [name]}]
              :symbol "symbol"
              :examples [{:code "(symbol :foo)"}]
              :type :function}
    "empty" {:description "Returns an empty collection of the same type as the argument. (empty nil) returns nil."
             :signature [{:params [coll]}]
             :symbol "empty"
             :examples [{:code "(empty [1 2 3])"}]
             :type :function}
    "intersection" {:description "Computes the intersection of one or more sets. 'nil' is treated as the empty set."
                    :signature [{:params [set & more]}]
                    :symbol "intersection"
                    :examples [{:code "(intersection #{1 2} #{2 3})"}]
                    :type :function}
    "when" {:description "Executes body expressions in an implicit do block if af and only if the test expression evaluates to a truthy value."
            :signature [{:params [test & body]}]
            :symbol "when"
            :examples [{:code "(when (some-condition) (rollback :this-went-wrong))"}]
            :type :macro}
    "account" {:description "Returns the Account record for a given addess, or nil if the account does not exist. Argument must cast to Address."
               :signature [{:params [address]}]
               :symbol "account"
               :examples [{:code "(account *address*)"}]
               :type :function}
    "set-memory" {:description nil, :signature nil, :symbol "set-memory", :examples nil}
    "symbol?" {:description "Tests if the argument is a Symbol."
               :signature [{:params [a]}]
               :symbol "symbol?"
               :examples [{:code "(symbol? 'foo)"}]
               :type :function}
    "compile" {:description "Compiles a form, returning an op."
               :signature [{:params [form]}]
               :symbol "compile"
               :examples [{:code "(compile '(fn [x] (* x 2)))"}]
               :type :function}
    "transfer" {:description "Transfers the specified amount of coins to the target-address. Returns the amount transferred if successful. Returns FUNDS error if there is insufficient balance in the sender's account. Returns STATE error if the reciever is an Actor that is unable to accept funds."
                :signature [{:params [address amount]}]
                :symbol "transfer"
                :examples [{:code "(transfer my-friend-address 12345678)"}]
                :type :function}
    "*aliases*" {:description "Returns the map of aliases in the current enviornment. Aliases enable symbols with namespaces e.g. 'library/foo' enables lookup of the symbol 'foo' via the 'library' alias."
                 :signature nil
                 :symbol "*aliases*"
                 :examples [{:code "*aliases*"}]
                 :type :special}
    "hash?" {:description "Tests if the given value is a Hash. A Hash is a specialised type of Blob"
             :signature [{:params [a]}]
             :symbol "hash?"
             :examples [{:code "(hash? (hash *state*))"}]
             :type :function}
    "*initial-expander*" {:description "The initial expander used to expand forms."
                          :signature [{:params [form cont]}]
                          :symbol "*initial-expander*"
                          :examples [{:code "(expand '(if 1 2 3) *initial-expander*)"}]
                          :type :expander}
    "undef" {:description "Undefines a symbol, removing the mapping from the surrent environment if it exists."
             :signature [{:params [sym]}]
             :symbol "undef"
             :examples [{:code "(do (def foo 1) (undef foo))"}]
             :type :macro}
    "and" {:description "Executes expressions in sequence, returning the first falsey value, or the last value if all were truthy. Does not evaluate later expressions, so can be used to short circuit execution. Returns true with no expressions present."
           :signature [{:params [& exprs]}]
           :symbol "and"
           :examples [{:code "(and 1 2)"}]
           :type :macro}
    "hash-set" {:description "Constructs a Set with the given values. If a value is repeated, it will be included only once in the set."
                :signature [{:params [& values]}]
                :symbol "hash-set"
                :examples [{:code "(hash-set 1 2 3)"}]
                :type :function}
    "set?" {:description "Tests if the agument is a Set."
            :signature [{:params [a]}]
            :symbol "set?"
            :examples [{:code "(set? #{1 2 3})"}]
            :type :function}
    "boolean?" {:description "Tests if the argument is a boolean (either true or false)."
                :signature [{:params [a]}]
                :symbol "boolean?"
                :examples [{:code "(boolean? false)"}]
                :type :function}
    "set-key" {:description nil, :signature nil, :symbol "set-key", :examples nil}
    "do" {:description "Executes multiple expressions sequentially, and returns the value of the final expression.."
          :signature [{:params [& expressions]}]
          :symbol "do"
          :examples [{:code "(do (count [1 2 3]) :done)"}]
          :type :special}
    "last" {:description "Returns the last element of a sequential collection, in collection-defined order. Collection argument must be coercible to a sequence."
            :signature [{:params [coll]}]
            :symbol "last"
            :examples [{:code "(last [1 2 3])"}]
            :type :function}
    "get-holding" {:description "Gets the holding value for a specified owner account address. Owner account must exist. Holding will be null by default."
                   :signature [{:params [owner]}]
                   :symbol "get-holding"
                   :examples [{:code "(get-holding *caller*)"}]
                   :type :function}
    "<" {:description "Tests if numeric arguments are in strict increasing order. Reads as 'less-than'."
         :signature [{:params [& xs]}]
         :symbol "<"
         :examples [{:code "(< 1 2 3)"}]
         :type :function}
    "set*" {:description "Sets or overwrites a value in the current local lexical bindings. Helper function for the 'set!', which should be preferred in most cases."
            :signature [{:params [symbol value]}]
            :symbol "set*"
            :examples [{:code "(set* 'a 10)"}]
            :type :function}
    "abs" {:description nil, :signature nil, :symbol "abs", :examples nil}
    "conj" {:description "Adds elements to a data structure, in the natural mode of addition for the data structure. Supports sequential collections, sets and maps."
            :signature [{:params [coll & elems]}]
            :symbol "conj"
            :examples [{:code "(conj [1 2] 3)"} {:code "(conj {1 2} [3 4])"} {:code "(conj #{1 2} 3)"}]
            :type :function}
    "double" {:description "Casts any numerical value to a Double."
              :signature [{:params [a]}]
              :symbol "double"
              :examples [{:code "(double 3)"}]
              :type :function}
    "meta" {:description "Returns metadata for a Syntax Object. Returns nil if the argument is not a Syntax Object."
            :signature [{:params [syntax]}]
            :symbol "meta"
            :examples [{:code "(meta (syntax 'foo {:bar 1}))"}]
            :type :function}
    "*origin*" {:description "Returns the Address of the account that initially signed this transaction. Usually, you should NOT use this for access control, since a rogue Actor can potentially trick a User into creating a transaction that allows code to be indirectly executed. Consider using *caller* for access control instead."
                :signature nil
                :symbol "*origin*"
                :examples [{:code "*origin*"}]
                :type :special}
    "syntax" {:description "Wraps a value as a Syntax Object, if it is not already a Syntax Object. If metadata is provided, merge the metadata into the resulting Syntax Object."
              :signature [{:params [value]} {:params [value meta]}]
              :symbol "syntax"
              :examples [{:code "(syntax 'bar)"}]
              :type :function}
    "defined?" {:description "Tests if a given symbol name is defined in the current environment."
                :signature [{:params [sym]}]
                :symbol "defined?"
                :examples [{:code "(defined? defined?)"}]
                :type :macro}
    "exports?" {:description "Tests if a specified actor exports a given symbol name."
                :signature [{:params [actor symbol]}]
                :symbol "exports?"
                :examples [{:code "(exports? actor-address 'function-name)"}]
                :type :function}
    "quote" {:description "Returns the quoted value of a form, without evaluating it. For example, you can quote a symbol to get the symbol itself rather than the value in the environment that it refers to."
             :signature [{:params [form]}]
             :symbol "quote"
             :examples [{:code "(quote foo)"} {:code "(eval (quote (+ 1 2 3)))"}]
             :type :special}
    "*holdings*" {:description "Returns the holdings map for this Account. Holdings are data values controlled by other Accounts (usually Actors, e.g. Token contracts). They can be used to indicate that an Account may have special rights or holdings with respect to a specific Actor, although this interpretation is Actor-specific."
                  :signature nil
                  :symbol "*holdings*"
                  :examples [{:code "*holdings*"}]
                  :type :special}
    "schedule*" {:description "Schedules a form for future execution under this account. Expands and compiles form now, but does not execute until the specified timestamp."
                 :signature [{:params [timestamp code]}]
                 :symbol "schedule*"
                 :examples [{:code "(schedule* (+ *timestamp* 1000) '(transfer my-friend 1000000))"}]
                 :type :macro}
    "zero?" {:description "Tests if the argument has the numeric value zero. Returns false for any non-numeric values."
             :signature [{:params [x]}]
             :symbol "zero?"
             :examples [{:code "(zero? 0.1)"}]
             :type :function}
    "rollback" {:description "Escapes from the currently executing smart contract. Rolls back any state changes. Returns the given value."
                :signature [{:params [val]}]
                :symbol "rollback"
                :examples [{:code "(rollback :aborted)"}]
                :type :special}
    "set" {:description "Coerces any collection to a Set."
           :signature [{:params [coll]}]
           :symbol "set"
           :examples [{:code "(set [1 2 3])"}]
           :type :function}
    "fn?" {:description "Tests if the argument is a function. Some arguments may be castable to functions but are not functions themselves, e.g. maps and vectors."
           :signature [{:params [a]}]
           :symbol "fn?"
           :examples [{:code "(fn? count)"}]
           :type :function}
    "def" {:description "Creates a definition in the current environment. This value will persist in the environment owned by the current account."
           :signature [{:params [sym value]}]
           :symbol "def"
           :examples [{:code "(def a 10)"}]
           :type :special}
    "coll?" {:description "Tests if the argument is a Collection. Collections include maps, vectors, lists, sets."
             :signature [{:params [a]}]
             :symbol "coll?"
             :examples [{:code "(coll? [1 2 3])"}]
             :type :function}
    "reduce" {:description "Reduces over a sequential collection, calling the function with the accumulated value and each element."
              :signature [{:params [f init coll]}]
              :symbol "reduce"
              :examples [{:code "(reduce (fn [acc x] (* acc x)) 1 [1 2 3 4 5])"}]
              :type :function}
    "*caller*" {:description "Returns the Address of the caller Account, or nil if this is a top-level User transaction. If an Actor or User makes an Actor call, they will be the caller for the duration of execution in the context of the called code."
                :signature nil
                :symbol "*caller*"
                :examples [{:code "*caller*"}]
                :type :special}
    "recur" {:description "Escapes from the currently executing code and recurs at the level of the next loop or function body."
             :signature [{:params [x y]}]
             :symbol "recur"
             :examples [{:code "(recur acc (dec i))"}]
             :type :special}
    "dotimes" {:description "Repeats execution of the body 'count' times, binding the specified symbol from 0 to (count-1) on successive iterations. Returns nil in all cases."
               :signature [{:params [[sym count] & body]}]
               :symbol "dotimes"
               :examples [{:code "(dotimes [i 10] (transfer *address* 10))"}]
               :type :macro}
    "assoc-in" {:description nil, :signature nil, :symbol "assoc-in", :examples nil}
    "assoc" {:description "Adds entries into an associative data structure, taking each two arguments as key/value pairs. A nil data structure is considered as an empty map."
             :signature [{:params [m & kvs]}]
             :symbol "assoc"
             :examples [{:code "(assoc {1 2} 3 4)"}]
             :type :function}
    "*address*" {:description "Returns the Address of the current Account. This will be the User address in a newly submitted transaction, or the Actor if an Actor call is being executed."
                 :signature nil
                 :symbol "*address*"
                 :examples [{:code "(address? *address*)"}]
                 :type :special}
    "mapv" {:description "Maps a function over the given collections as with map, and returns the resturn as a vector."
            :signature [{:params [f & colls]}]
            :symbol "mapv"
            :examples [{:code "(mapv inc '(1 2 3))"}]
            :type :function}
    "into" {:description "Adds elements to a collection, in a collection-defined manner as with 'conj'."
            :signature [{:params [coll elements]}]
            :symbol "into"
            :examples [{:code "(into {} [[1 2] [3 4]])"}]
            :type :function}
    "when-not" {:description "Executes body expressions in an implicit do block if af and only if the test expression evaluates to a falsey value."
                :signature [{:params [test & body]}]
                :symbol "when-not"
                :examples [{:code "(when-not (some-condition) (return :some-result))"}]
                :type :macro}
    "dec" {:description "Decrements the given number by 1. Converts to Long if necessary."
           :signature [{:params [num]}]
           :symbol "dec"
           :examples [{:code "(dec 10)"}]
           :type :function}
    "disj" {:description "Removes a key from a set. If the key does not exist, returns the same set unchanged."
            :signature [{:params [coll key]}]
            :symbol "disj"
            :examples [{:code "(disj #{1 2 3} 1)"}]
            :type :function}
    "difference" {:description "Computes the difference of one or more sets. 'nil' is treated as the empty set."
                  :signature [{:params [set & more]}]
                  :symbol "difference"
                  :examples [{:code "(difference #{1 2} #{2 3})"}]
                  :type :function}
    "==" {:description "Tests if arguments are equal in numerical value."
          :signature [{:params [& xs]}]
          :symbol "=="
          :examples [{:code "(== 2 2.0)"}]
          :type :function}
    "get" {:description "Gets an element from a collection at the specified index value. Works on all collection types including maps, sets and sequences. Nil is treated as an empty collection. If the index is not present, returns not-found value (or nil if not-found is not provided)."
           :signature [{:params [coll key]} {:params [coll key not-found]}]
           :symbol "get"
           :examples [{:code "(get {:foo 10 :bar 15} :foo)"}]
           :type :function}
    "merge" {:description "Merges zero or more hashmaps, replacing existing values. nil is considered as an empty map."
             :signature [{:params [& maps]}]
             :symbol "merge"
             :examples [{:code "(merge {1 2 3 4} {3 5 7 9})"}]
             :type :function}
    "blob?" {:description "Tests if the argument is a blob."
             :signature [{:params [a]}]
             :symbol "blob?"
             :examples [{:code "(blob? some-val)"}]
             :type :function}
    "*memory*" {:description "Gets the current Memory Allowance for this Account. May be zero - in which case any new memory allocations will be charged at the current memory exchange pool price."
                :signature nil
                :symbol "*memory*"
                :examples [{:code "*memory*"}]
                :type :special}
    "+" {:description "Adds numerical arguments. Result will be a Long if all all integer, or a double if any floating point values are included."
         :signature [{:params [& xs]}]
         :symbol "+"
         :examples [{:code "(+ 1 2 3)"}]
         :type :function}
    "store" {:description "Stores a value permanently in immutable storage, and returns its hash ID. 'store' is useful primarily for data structures that you expect to be widely shared, but don't want to incur the memory costs of making many copies. Using 'store' and 'fetch' you can store the data just once, and access it by reference."
             :signature [{:params [value]}]
             :symbol "store"
             :examples [{:code "(store [1 2 3 4])"}]
             :type :function}
    "*sequence*" {:description "Returns the current sequence number for this account. The sequence number is equal to the number of signed transactions executed. The next valid sequence number is (*sequence* + 1)."
                  :signature nil
                  :symbol "*sequence*"
                  :examples [{:code "*sequence*"}]
                  :type :special}
    "actor" {:description "Creates an actor generation function. This function generates depolyable code when called, and may take optional arguments to configure the deployed actor. The resulting code can be used with either 'deploy' or 'deploy-once'."
             :signature [{:params [name params & body]}]
             :symbol "actor"
             :examples [{:code "(let [agf (actor [x] (defn calc [y] (* x y)) (export calc))] (deploy (agf 13)))"}]
             :type :macro}
    "lookup-syntax" {:description "Looks up the Syntax Object for a symbol in the current execution environment, or the account of the given address if specified. Name may be a symbol, keyword or String. Returns nil if not found."
                     :signature [{:params [name]} {:params [address name]}]
                     :symbol "lookup-syntax"
                     :examples [{:code "(lookup-syntax 'count)"}]
                     :type :function}
    "doc" {:description "Returns the documentation for a given definition."
           :signature [{:params [sym]}]
           :symbol "doc"
           :examples [{:code "(doc count)"}]
           :type :macro}
    "assert" {:description "Evaluates each test, and raises an ASSERT error if any are not truthy."
              :signature [{:params [& tests]}]
              :symbol "assert"
              :examples [{:code "(assert (= owner *caller*))"}]
              :type :macro}
    "byte" {:description "Casts a value to a Byte. Discards high bits of larger integer types."
            :signature [{:params [a]}]
            :symbol "byte"
            :examples [{:code "(byte 1234)"}]
            :type :function}
    "*state*" {:description "Returns the current CVM state record. This is a very large object, and should normally only be used temporarily to look up relevant values."
               :signature nil
               :symbol "*state*"
               :examples [{:code "(keys *state*)"} {:code "(get-in *state* [:accounts *address* :balance])"}]
               :type :special}
    "macro" {:description "Creates a macro expansion procedure. When used in a function call position, the macro procedure will expand to the given expression, which should be a compilable form."
             :signature [{:params [params & body]}]
             :symbol "macro"
             :examples [{:code "(macro [x] '(or x y))"}]
             :type :macro}
    "blob-map" {:description "Creates an empty BlobMap. Blobmaps support blob types as keys only."
                :signature [{:params []}]
                :symbol "blob-map"
                :examples [{:code "(blob-map)"}]
                :type :function}
    "fail" {:description "Causes execution to fail at the current position. Error type defaults to ASSERT if not specified. Error message defaults to nil if not specified. The message may be any value, but the use of short descriptive strings is recommended."
            :signature [{:params []} {:params [message]} {:params [error-type message]}]
            :symbol "fail"
            :examples [{:code "(fail :ASSERT \"Assertion failed\")"}]
            :type :function}
    "*juice*" {:description "Returns the amount of execution juice remaining at this point of the current transaction. Juice is required for every CVM operation executed, and the transaction will fail immediately with a JUICE error if an attempt is made to consume juice beyond this value."
               :signature nil
               :symbol "*juice*"
               :examples [{:code "*juice*"}]
               :type :special}
    "first" {:description "Returns the first element from a collection. Will cause a BOUNDS error if the collection is empty. It can be a good idea to check for this case with `empty?` first."
             :signature [{:params [coll]}]
             :symbol "first"
             :examples [{:code "(first [1 2 3])"}]
             :type :function}
    "signum" {:description nil, :signature nil, :symbol "signum", :examples nil}
    "defmacro" {:description "Defines a macro in the current environment."
                :signature [{:params [a]}]
                :symbol "defmacro"
                :examples [{:code "(defmacro foo [x] '(if (good? x) x (fix x)))"}]
                :type :macro}
    "eval-as" {:description nil, :signature nil, :symbol "eval-as", :examples nil}
    ">" {:description "Tests if numeric arguments are in strict decreasing order. Reads as 'greater-than'."
         :signature [{:params [& xs]}]
         :symbol ">"
         :examples [{:code "(> 3 2 1)"}]
         :type :function}})

(defn word-at-position [^js document position]
  (when-let [range (.getWordRangeAtPosition document position)]
    (.getText document range)))

(defn- register-disposable [^js context ^js disposable]
  (-> (.-subscriptions context)
      (.push disposable)))

(defn dispose [^js context & disposables]
  (doseq [disposable disposables]
    (register-disposable context disposable)))

(deftype ConvexHoverProvider []
  Object
  (provideHover [_ document position _]
    (let [word-at-position (word-at-position document position)]
      (when-let [{:keys [symbol description signature examples]} (convex-core-reference word-at-position)]
        (let [^js markdown (doto (vscode/MarkdownString. (str "**" symbol "**"))
                             (.appendCodeblock (str/join "\n" (map :params signature)) "convex")
                             (.appendText "\n\n")
                             (.appendMarkdown description)
                             (.appendText "\n\n")
                             (.appendMarkdown "**Examples**\n")
                             (.appendCodeblock (str/join "\n" (map :code examples)) "convex")
                             (.appendText "\n\n"))]

          (.appendMarkdown markdown (str "[See on convex.world](https://convex.world/documentation/reference)"))

          (vscode/Hover. markdown))))))

(deftype ConvexDocumentRangeFormattingEditProvider []
  Object
  (provideDocumentRangeFormattingEdits [_ ^js document range options token]
    (let [configuration {:indentation?                    true
                         :remove-surrounding-whitespace?  true
                         :remove-trailing-whitespace?     true
                         :insert-missing-whitespace?      true
                         :remove-consecutive-blank-lines? false}

          text (.getText document range)

          pretty (try
                   (cljfmt/reformat-string text configuration)
                   (catch js/Error e
                     (js/console.log (.-message e))))]

      (when pretty
        #js [(vscode/TextEdit.replace range pretty)]))))

(def word-pattern
  "Convex symbol regex."
  #"(?:/|[^\s,;\(\)\[\]{}\"`~@\^\\][^\s,;\(\)\[\]{}\"`~@\^\\]*)")

(def document-selector
  #js {:language "convex"})

(defn activate [^js context]
  (let [^js languages (.-languages vscode)]
    (.setLanguageConfiguration languages "convex" #js {:wordPattern word-pattern})

    (dispose context
             (.registerHoverProvider languages "convex" (ConvexHoverProvider.))
             (.registerDocumentRangeFormattingEditProvider languages document-selector (ConvexDocumentRangeFormattingEditProvider.)))))

(defn deactivate []
  nil)