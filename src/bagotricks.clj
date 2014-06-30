(ns bagotricks
  "Collection of useful Clojure functions."
  (:require [clojure.java.io       :as io]
            [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)



;; The following is from Prismatic's Plumbing library and maintains it's original license
;; Details: https://github.com/Prismatic/plumbing
;; Source:  https://github.com/Prismatic/plumbing/blob/master/src/plumbing/core.clj
;;

(defmacro fn->
  "Equivalent to `(fn [x] (-> x ~@body))"
  [& body]
  `(fn [x#] (-> x# ~@body)))

(defmacro fn->>
  "Equivalent to `(fn [x] (->> x ~@body))"
  [& body]
  `(fn [x#] (->> x# ~@body)))

(defmacro <-
  "Converts a ->> to a ->

   (->> (range 10) (map inc) (<- doto prn) (reduce +))

   Jason W01fe is happy to give a talk anywhere any time on
   the calculus of arrow macros.

   Note: syntax modified from original."
  ;; [& body] ;; original version
  ;; `(-> ~(last body) ~@(butlast body)) ;; original version
  ([x] `(~x))
  ([cmd & body]
      `(~cmd ~(last body) ~@(butlast body))))


;; Fix if-let
;;

(defmacro ^{:private true} if-lets*
  [bindings then else]
  (let [form (subvec bindings 0 2)
        more (subvec bindings 2)]
    (if (empty? more)
      `(if-let ~form
         ~then
         ~else)
      `(if-let ~form
         (if-lets* ~more ~then ~else)
         ~else))))

(defmacro if-lets
  "Like if-let, but accepts multiple bindings and evaluates them sequentally.
   binding evaluation halts on first falsey value, and 'else' clause activates."
  ([bindings then]
     `(if-lets ~bindings ~then nil))
  ([bindings then else]
     (cond
      (not (even? (count bindings))) (throw (IllegalArgumentException. "if-lets requires an even number of bindings"))
      (not (vector? bindings))       (throw (IllegalArgumentException. "if-lets requires a vector for its binding"))
      :else `(if-lets* ~bindings ~then ~else))))


;; Handle types
;; 

(defprotocol ToInt 
  (to-int [i] "A simple function to coerce numbers, and strings, etc; to an int.
   Note: nil input returns nil."))

(extend-protocol ToInt
  java.lang.Integer
  (to-int [i] i)
  
  java.lang.Long
  (to-int [i] (int i))
  
  java.lang.Double
  (to-int [i] (int i))
  
  java.lang.Float
  (to-int [i] (int i))

  nil
  (to-int [_] nil)

  java.lang.String
  (to-int [i] (Integer/parseInt i)))

(defprotocol ToLong  
  (to-long [i] "A simple function to coerce numbers, and strings, etc; to a long.
   Note: nil input returns nil."))

(extend-protocol ToLong
  java.lang.Integer
  (to-long [l] (long l))
  
  java.lang.Long
  (to-long [l] l)
  
  java.lang.Double
  (to-long [l] (long l))
  
  java.lang.Float
  (to-long [l] (long l))

  nil
  (to-long [_] nil)

  java.lang.String
  (to-long [l] (Long/parseLong l)))

(defn okay-string?
  "Returns true if string not nil or empty"
  [^String str]
  (and str 
       (not (.isEmpty str))))

(defn std-keyword [^String s]
  "Convert string to a standard Clojure keyword
   Ex. TITLE_NAME -> :title-name" 
  (-> s
      .toLowerCase
      (.replace \_ \-)
      (.replace \  \-)
      keyword))



;; Handle Strings
;;

(defn re-get [regex ^String string]
  "Return the matched portion of a regex from a string"
  (let [[_ & xs] (re-find regex string)]
    xs))

(defn get-char-blocks
  "For a given string, list the unicode blocks for each character"
  [^String str]
  (map (fn [^Character c]
         (. (java.lang.Character$UnicodeBlock/of (. c charValue)) toString))
       str))

(defn has-kanji?
  "See if the string contains a kanji"
  [^String str]
  (some #(= % "CJK_UNIFIED_IDEOGRAPHS") (get-char-blocks str)))

(defn is-latin?
  "See if the string is all latin"
  [^String str]
  (every? #{"BASIC_LATIN" "LATIN_1_SUPPLEMENT"}
          (get-char-blocks str)))

(defn is-yomi? [^String s]
  "Return true if is 'yomi'"
  (re-find #"^[ァ-ヴ　 ・ー~]+$" s))

(defn split-tsv [^String s]
  "Parse a line of a TSV file and return nil for fields which are empty"
  (->> (clojure.string/split s #"\t" -1)
       (mapv (fn [^String x]
               (if (.isEmpty x) nil x)))))

(defn morph [input_format output_format kw]
  "Provide an example of the input where 'FOO' is variable, and an example of the output where 'FOO' is the variable input;
   and will convert the provided keyword.

   Ex. (morph \"MY_FOO_NAME\" \"FOO_SCORE\" \"MY_ARTIST_NAME\") -> \"ALBUM_SCORE\"
       or
       (morph :MY_FOO_NAME :FOO_SCORE :MY_ARTIST_NAME) -> :ALBUM_SCORE"
  (let [[in-pre in-post]   (re-get #"(.*)FOO(.*)" (str input_format))
        [out-pre out-post] (re-get #"(.*)FOO(.*)" (str output_format))
        foo-value          (first (re-get (re-pattern (str in-pre "(.*)" in-post))
                                          (str kw)))]
    (if (= (type output_format) clojure.lang.Keyword)
      (keyword (str (name out-pre) foo-value out-post))
      (str out-pre foo-value out-post))))



;; Handle OS/Java wrappers
;; 

(defmacro thread
  "Execute the following S-EXP in another thread, and return thread"
  [sexp]
  `(let [thread# (java.lang.Thread. (fn [] ~sexp))]
     (.start thread#)
     thread#))

(defn get-os []
  "Return :linux , :mac , or :windows identifying the current OS"
  (condp re-get (System/getProperty "os.name")
      #"(?i)^(lin)" :linux
      #"(?i)^(mac)" :mac
      #"(?i)^(win)" :windows))

(defn exec [^String cmd]
  "Execute the provided command"
  (-> (Runtime/getRuntime)
      (.exec cmd)))

(defn launch-url [^String url]
  "launch the provided url in a system browser"
  (if (and (java.awt.Desktop/isDesktopSupported)
           (-> (java.awt.Desktop/getDesktop)
               (.isSupported java.awt.Desktop$Action/BROWSE)))
    (-> (java.awt.Desktop/getDesktop)
        (.browse (java.net.URI. (.toString url))))
    (throw (Exception. (str "Sorry java.awt.Desktop, or a browser, isn't setup via Mac/Win/libgnome so you're on your own. Type the following into a browser, " url)))))

(defn get-queue-lbq
  "Generates a java.util.concurrent.LinkedBlockingQueue
  and returns two functions for 'put' and 'take'"
  ([]
     (let [bq   (java.util.concurrent.LinkedBlockingQueue.)
           put  #(.put bq %)
           take #(.take bq)]
       [put take]))
  ([col]
     (let [bq   (java.util.concurrent.LinkedBlockingQueue. ^Integer (to-int col))
           put  #(.put bq %)
           take #(.take bq)]
       [put take])))



;; Handle IO 
;;

(defn read-lines [^String filename]  
  (with-open [x (io/reader filename)]
    (vec (line-seq x))))

(defn dump-lines-to-file [filename xs]
  "Provided a list of strings, output each to a line in the provided filename."
  (with-open [wtr (clojure.java.io/writer filename)]
    (binding [*out* wtr]
      (doseq [x xs]
        (println x)))))

(defn dump-recs-to-file [filename xs]
  "Provided a list of lists, output a tab-delimited file to filename."
  (dump-lines-to-file filename
                      (map #(clojure.string/join "\t" %) xs)))



;; Handle data processing
;; 

(defn fold-into-vec
  "Provided a reducer, concatenate into a vector.
   Note: same as (into [] coll), but parallel."
  ([coll]   (r/fold   (r/monoid into vector) conj coll))
  ([n coll] (r/fold n (r/monoid into vector) conj coll)))

(defn fold-into-lazy-seq
  "Accept a reducible sequence, and produce a lazy-seq of it's results.
   Order *NOT* guarranteed!"
  ([rseq] (fold-into-lazy-seq nil rseq))
  ([q-size rseq]
     (let [q    (if q-size
                  (java.util.concurrent.LinkedBlockingQueue. ^Integer (to-int q-size))
                  (java.util.concurrent.LinkedBlockingQueue.))
           put  #(.put  q %)
           take #(.take q)
           error-list    (atom []) ;; To store any exceptions that may occur
           sentinel      (java.util.UUID/randomUUID)  ;; So we can tell when we're done
           nil-surrogate (java.util.UUID/randomUUID)] ;; LinkedBlockingQueue wont accept NULL values, so...

       ;; Enqueue data in another thread
       (thread (try (->> rseq
                         (r/map (fn [x]
                                  (put (if (nil? x)
                                         nil-surrogate
                                         x))
                                  1))
                         (r/fold +))
                    (catch Exception e ;; Record any exceptions
                      (swap! error-list conj e))
                    (finally ;; Add final sentinel
                     (put sentinel))))

       ;; Produce lazy-seq
       (take-while #(or (not= % sentinel)
                        (if-not (empty? @error-list)
                          (throw (first @error-list))))
                   (repeatedly #(let [v (take)]
                                  (if (= v nil-surrogate)
                                    nil
                                    v)))))))

(defn megamap
  "Like pmap, but is not lazy and will process elements out of order.
  If f returns a value, then to return items in order the entire set
  will be sorted and held in memory.
  When the time of f varies significantly, megamap will be dramatically
  faster than pmap for handling the entire list.

  NOTE: You very likely should be using reducers instead here..."
  [f col]
  (let [proc-count (+ 2 (.. Runtime getRuntime availableProcessors))
        que        (java.util.concurrent.ConcurrentLinkedQueue. (map (fn [a b] [a b]) (range) col))
        result     (java.util.concurrent.ConcurrentLinkedQueue.)]
    (doall (map (fn [^java.lang.Thread t] (.join t))
                (doall (map (fn [_] (thread (loop [[k v] (.poll que)]
                                             (if k
                                               (do 
                                                 (if-let [res (f v)]
                                                   (.add result [k res]))
                                                 (recur (.poll que)))))))
                            (range proc-count)))))
    (if-let [results (seq (.toArray result))]
      (->> results
           (sort-by first)
           (map second)))))



;; Handle data structures
;; 

(defn index-by-fn
  "Return a map indexed by the value of key-fn applied to each element in data.
   Note: key-fn can return a collection of multiple keys."
  ([key-fn data]
     (index-by-fn key-fn identity data))
  ([key-fn value-fn data]
     (persistent! (reduce (fn [ndx elem]
                            (let [key (key-fn elem)]
                              
                              ;; Handle the case where key-fn returns multiple keys
                              (if (coll? key)
                                (reduce (fn [sndx selem] (assoc! sndx selem (conj (sndx selem) (value-fn elem)))) ndx key)
                                
                                ;; Handle the case where key-fn returns one key
                                (assoc! ndx key (conj (ndx key) (value-fn elem))))))
                          (transient {})
                          data))))

(defn strip-nil
  "Take a hash, and return the hash minus any elements with a nil value. 
   If nil is provided, then nil is returned."
  [x]
  (when x
    (->> x
         (reduce (fn [foo [k v]] (if (nil? v)
                                   foo
                                   (assoc! foo k v)))
                 (transient {}))
         (persistent!))))

(defn seq-n-grams
  "Return various 'n-grams' for sequence"
  ([xs] (seq-n-grams (count xs) xs))
  ([max-size-p xs] (let [xs-size  (count xs)
                         max-size (or max-size-p xs-size)
                         xs-vec   (into [] xs)]
                     (for [s (range xs-size)
                           e (range (inc xs-size))
                           :when (and (> e s)
                                      (<= (- e s) max-size ))]
                       (subvec xs-vec s e)))))

(defn most?
  "Like 'every?' and 'some' but for 'most' items in coll.
   Most defaults to 0.8 for 80% true."
  ([f coll] (most? f 0.8 coll))
  ([f percent coll]
     (let [all  (count coll)
           good (count (filter f coll))]
       (if (zero? all)
         false
         (> (/ good all) percent)))))

(defn filter-in [filter-spec record]
  "Filter a map, or vector of maps, by a filter-spec.
   If :filter-in/complement? is true, then the complement is returned.

   Ex.
      (def foo {:a 1 :b {:c 2 :d 3}})

      (filter-in {:a true :b {:d true}} foo)
      ->
      {:a 1, :b {:d 3}}

      (filter-in #{:b :filter-in/complement?} foo)
      ->
      {:a 1}"
  (let [ ;; Define's keyword for filter-in negation option
        option-key    :filter-in/complement?
        default       (filter-spec option-key)

        ;; Don't assoc nil values
        assoc-no-nil! (fn [m k v] (if v
                                    (assoc! m k v)
                                    m))

        ;; fn which XOR's against filter-spec param
        xor (if default
              not
              identity)

        ;; assoc! based on results of filter-spec and negation option
        filter-fn (fn [switch m k v]
                    (if (xor switch)
                      (assoc-no-nil! m k v)
                      m))

        ;; Subspec's inheret negation options from parents
        default-subspec (fn [subspec]
                          (if (map? subspec)
                            (merge {option-key default} subspec)
                            (if default
                              (conj subspec option-key)
                              subspec)))

        ;; Apply filter-spec to individual record
        work-fn     (fn->> (reduce (fn [agg [k v]]
                                     (let [ans (filter-spec k)]
                                       (if (coll? ans)
                                         (assoc-no-nil! agg k (filter-in (default-subspec ans) v))
                                         (filter-fn ans agg k v))))
                                   (transient {}))
                           persistent!
                           (#(when-not (empty? %) %)))]
    (if (vector? record)
      (->> record
           (keep work-fn)
           vec)
      (work-fn record))))

;; Misc
;;

(defmacro percent-chance
  "Branch true/false based on a percentage of chance"
  [chance lucky & unlucky]
  `(if (<= (* (rand) 100) ~chance)
     ~lucky
     ~@unlucky))

(defmacro cond-percent*
  [random-percent & clauses]
  (let [cond-details (->> clauses
                          (partition 2 2 nil)
                          (sort-by first >)
                          (reduce (fn [[agg total] [percent clause]]
                                    (if (not (and percent clause))
                                      (throw (IllegalArgumentException. "cond-percent requires an even number of forms"))
                                      (let [nval (+ percent total)]
                                        [(conj agg
                                               (list clojure.core/<
                                                     random-percent
                                                     nval)
                                               clause)
                                         nval])))
                                  [['clojure.core/cond] 0]))]
    (if (== (second cond-details) 100)
      (-> cond-details
          first
          seq)
      (throw (IllegalArgumentException.
              "cond-percent requires percent clauses sum to 100%")))))


(defmacro cond-percent
  "Similar to clojure.core/cond, but for each condition takes the percentage chance
   the form should be executed and returned.

   Ex. (cond-percent
         50 \"50% Chance\"
         40 \"40% Chance\"
         10 (str 10 \"% Chance\")

   NOTE: all conditions must sum to 100%"
  [& clauses]
  `(let [random-percent# (* (clojure.core/rand) 100)]
     (cond-percent* random-percent# ~@clauses)))

(defn get-random-elem
  "Provided a element distribution, choose an element randomly along the distribution"
  [distribution]
  (let [random-percent (* (clojure.core/rand)
                          100)
        cdf (->> distribution
                 (sort-by second >)
                 (reduce (fn [[agg total] [elem dist]]
                           (let [nval (+ total dist)]
                             [(conj agg [elem nval]) nval]))
                         [[] 0])
                 first)]
    (if (== (-> cdf last second) 100)
      (->> cdf
           (drop-while #(< (second %) random-percent))
           first
           first)
      (throw (IllegalArgumentException.
              "element distribution requires percent clauses sum to 100")))))



(set! *warn-on-reflection* false)
