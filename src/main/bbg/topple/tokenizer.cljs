(ns bbg.topple.tokenizer
  (:require [cljs.reader :as reader]
            [bbg.util :refer [when-pred]]))

(defn- chars-read? [{:keys [pos start] :as _l}]
  (> pos start))

(defn- next-char
  ([l] (next-char l 1))
  ([{:keys [input pos] :as _l} n]
   (let [end (+ pos n)]
     (cond
       (> end (count input)) :eos
       :else                 (subs input pos end)))))

(defn- read-char
  "Advance `:pos` by `n` while not exceeding `(count input)`."
  ([l] (read-char l 1))
  ([{:keys [input pos] :as l} n]
   (assoc l :pos (min (+ pos n) (count input)))))

(defn unread-char
  ([l] (unread-char l 1))
  ([{:keys [pos] :as l} n]
   (assoc l :pos (max (- pos n) 0))))

(defn- emit-token
  "Add token to `:tokens` with `:t` `t` and `:v` the substring from
  `:input` based on the current `:pos` and `:start` "
  ([l t] (emit-token l t identity))
  ([{:keys [start pos input] :as l} t f]
   (let [token {:start start :t t :v (subs input start pos)}]
     #_(prn :token token)
     (update l :tokens conj (f token)))))

(defn- reset-start
  "Sets `:start` to the value of `:pos`."
  [{:keys [pos] :as l}]
  (assoc l :start pos))

(defn- skip-while [l pred]
  (loop [l l]
    (let [nc (when-pred string? (next-char l))]
      (if-not (pred nc)
        l
        (recur (-> l (read-char) (reset-start)))))))

;; state functions
(declare lex-text read-pipe read-filter)

(defn- read-right-meta [l]
  (-> l (read-char 2) (emit-token :right-meta) (reset-start) (assoc :state #'lex-text)))

(defn- literal-type [s]
  (cond
    (re-find #"^\"" s)    :string
    (re-find #"^[-\d]" s) :integer
    (re-find #"^#" s)     :regex
    (re-find #"[a-z]" s)  :symbol))

(defn- read-literal-dispatch [l]
  (literal-type (next-char l)))

(defmulti parse-literal #'literal-type)

(defmethod parse-literal :integer [v]
  (parse-long v))

(defmethod parse-literal :string [v]
  (reader/read-string v))

(defmethod parse-literal :regex [v]
  (re-pattern (parse-literal (subs v 1))))

(defmethod parse-literal :symbol [v]
  (reader/read-string v))

(defmulti read-literal #'read-literal-dispatch)

(defmethod read-literal :string [l]
  (loop [l       (read-char l)
         escapes 0]
    (let [nc      (when-pred string? (next-char l))
          escape? (= nc "\\")
          quote?  (= nc "\"")]
      (if (or (nil? nc)
              (and (zero? escapes) quote?))
        (read-char l)
        (recur (read-char l) (if escape? (inc escapes) 0))))))

(defmethod read-literal :integer [l]
  (loop [l l]
    (if-not (some->> l (next-char) (when-pred string?) (re-find #"^\d"))
      l
      (recur (read-char l)))))

(defmethod read-literal :regex [l]
  (-> l
      (read-char)
      (read-literal :string)))

(defmethod read-literal :symbol [l]
  (loop [l l]
    (if-not (some->> l (next-char) (when-pred string?) (re-find #"^[a-z]"))
      l
      (recur (read-char l)))))

(defn read-filter-arg [l]
  (-> l
      (read-literal)
      (emit-token :filter-arg (fn [{v :v :as item}]
                                (assoc item
                                       :v/parsed (parse-literal v)
                                       :v/type (literal-type v))))
      (reset-start)
      (assoc :state #'read-filter)))

(defn- read-filter [l]
  (cond
    (= ":" (next-char l))    (cond-> l
                               (chars-read? l) (emit-token :filter)
                               :always         (-> (read-char) ;; read ":"
                                                   (reset-start)
                                                   (assoc :state #'read-filter-arg)))
    (= "|" (next-char l))    (-> l (emit-token :filter) (reset-start) (assoc :state #'read-pipe))
    (= "}}" (next-char l 2)) (cond-> l
                               (chars-read? l) (emit-token :filter)
                               :always         (-> (reset-start) (assoc :state #'read-right-meta)))
    :else                    (read-char l)))

(defn- read-right-teta [l]
  (-> l
      (read-char 2)
      (emit-token :right-teta)
      (reset-start)
      (assoc :state #'lex-text)))

(defn- read-block-fn-arg [l]
  ;; "if-matches #\"\" a %}"
  ;; "if-matches %}"
  ;;             ^
  (cond
    (= "%}" (next-char l 2)) (-> l (assoc :state #'read-right-teta))
    :else                    (-> l
                                 (read-literal)
                                 (emit-token :block-fn-arg (fn [{v :v :as item}]
                                                           (assoc item
                                                                  :v/parsed (parse-literal v)
                                                                  :v/type (literal-type v))))
                                 (reset-start)
                                 (skip-while #{\space}))))

(defn- read-block-fn [l]
  ;; "{% if-matches %}"
  ;;     ^
  (let [nchars                (when-pred string? (next-char l 2))
        read?                 (chars-read? l)
        right-teta-coming-up? (= "%}" nchars)
        block-fn-read?        (and read? (= " " (first nchars)))]
    (cond
      (or right-teta-coming-up?
          block-fn-read?)
      (-> l
          (emit-token :block-fn (fn [{:keys [v] :as item}]
                                  (let [block-type (cond
                                                     (re-find #"^end-" v) :end
                                                     (= "else" v)         :else
                                                     :else                :start)]
                                    (assoc item :block/type block-type))))
          (reset-start)
          (skip-while #{\space})
          (assoc :state #'read-block-fn-arg))
      :else (read-char l))))

(defn- read-pipe [l]
  (-> l read-char (emit-token :pipe) (reset-start) (assoc :state #'read-filter)))

(defn- read-var [l]
  (cond
    (= "|" (next-char l))    (-> l (emit-token :var) (reset-start) (assoc :state #'read-pipe))
    (= "}}" (next-char l 2)) (-> l (emit-token :var) (reset-start) (assoc :state #'read-right-meta))
    :else                    (read-char l)))

(defn- read-left-meta [l]
  (-> l
      (read-char 2)
      (emit-token :left-meta)
      (reset-start)
      (assoc :state #'read-var)))

(defn- read-left-teta [l]
  (-> l
      (read-char 2)
      (emit-token :left-teta)
      (reset-start)
      (skip-while #{\space}) ;; trailing space acceptable, e.g. "{%  uppercase %}"
      (assoc :state #'read-block-fn)))

(defn- lex-text [l]
  (let [chars-read? (> (:pos l) (:start l))]
    #_(prn :chars-read? chars-read?)
    (cond
      (= :eos (next-char l))   (cond-> l
                                 chars-read? (-> (emit-token :text) reset-start)
                                 :always     (dissoc :state))
      (= "{{" (next-char l 2)) (cond-> l
                                 chars-read? (-> (emit-token :text) reset-start)
                                 :always     (assoc :state #'read-left-meta))
      (= "{%" (next-char l 2)) (cond-> l
                                 chars-read? (-> (emit-token :text) reset-start)
                                 :always     (assoc :state #'read-left-teta))
      :else                    (read-char l))))

(defn run [l]
  (loop [{state :state :as l} l]
    (if state
      (recur (doto (state l) #_prn))
      l)))

(defn- collect-args [tokens {:keys [type in] :or {in :args}}]
  (reduce (fn [acc {:keys [t v/parsed] :as item}]
            (let [last-ix (dec (count acc))]
              (if (= t type)
                (update-in acc [last-ix in] (fnil conj []) parsed)
                (conj acc item)))) [] tokens))

(defn tokenize*
  "Yields the lexer that results from applying `run` to an initial lexer."
  [s]
  (-> {:state #'lex-text :input s :start 0 :pos 0 :tokens []}
      run))

(defn- collect-blocks
  "This collects all block-related into 1 item per block."
  [tokens]
  (let [map-entry->pred  (fn [[k v]]
                           (comp (if (fn? v) ;; ensuring e.g. `:foo` is not seen as function
                                   v
                                   #(= % v)) k))
        query->filter-fn #(apply every-pred (map map-entry->pred %))
        find-tokens      (fn [query tokens] ;; query e.g. {:start #(< % 10) :t :block-fn}
                           (filterv (query->filter-fn query) tokens))]
    (if-some [block-start-tokens (not-empty (find-tokens {:block/type :start} tokens))]
      (let [start-token->end&else-query        (fn [{:keys [v next-start]}]
                                                 (let [end-v (str "end-" v)]
                                                   {:t     :block-fn
                                                    :v     (some-fn #(= % "else")
                                                                    #(= % end-v))
                                                    :start #(< % next-start)}))
            ;; Every block-start-token coupled with the next
            ;; block-start-token.
            ;;
            ;; When interleaving an uneven number of
            ;; block-start-tokens, we need to come up with the 'next
            ;; block-start-token' of the last token. This item should
            ;; be 'out of range' in terms of its `:start` to not
            ;; interfere with any real block that ends at the last
            ;; token.
            block-start-tokens-interleaved     (let [last-item {:start (-> tokens last :start inc)}]
                                                 (partition 2 1 (list last-item) block-start-tokens))
            block-start-tokens                 (map (fn assoc-next-start [[start-token {next-start :start}]]
                                                      (assoc start-token :next-start next-start))
                                                    block-start-tokens-interleaved)
            start-token->start-end-else-triple (fn [start-token]
                                                 ;; triple: [start ?end ?else]
                                                 (let [end-else (-> start-token
                                                                    start-token->end&else-query
                                                                    (find-tokens tokens)
                                                                    reverse)]
                                                   (cons start-token end-else)))
            block-start-end-else-triples       (map start-token->start-end-else-triple block-start-tokens)
            blocks                             (map (fn [[{open-start :start :as start-item} {close-start :start} {else-start :start}]]
                                                      (let [if-tokens-query   {:start (every-pred #(< open-start %) ;; exclude open-item
                                                                                                  #(if else-start
                                                                                                     (< % else-start)    ;; else-token will be in else-tokens
                                                                                                     (<= % close-start)))}
                                                            else-tokens-query {:start (every-pred #(<= % close-start)
                                                                                                  #(<= else-start %))}
                                                            [if-tokens
                                                             else-tokens
                                                             :as tokens]      (cond-> [(find-tokens if-tokens-query tokens)]
                                                                                else-start (conj (find-tokens else-tokens-query tokens)))
                                                            block-end         (or (some-> (or else-tokens if-tokens) last :start) open-start)]
                                                        (-> start-item
                                                            (assoc :block/items tokens)
                                                            (assoc :block/start open-start)
                                                            (assoc :block/end block-end)))) block-start-end-else-triples)
            in-block-tokens-range-pred         (fn [{:block/keys [start end]}] #(<= start % end))
            token-not-in-block-query           {:start (complement (apply some-fn (map in-block-tokens-range-pred blocks)))}]
        (into blocks (find-tokens token-not-in-block-query tokens)))
      tokens)))

(defn semantify [tokens]
  (-> tokens
      (collect-args {:type :filter-arg :in :filter/args})
      (collect-args {:type :block-fn-arg :in :block/args})
      collect-blocks
      (->> (sort-by :start))))

(defn tokenize [s]
  (-> (tokenize* s)
      :tokens
      semantify))

(comment
  (def init-state {:state #'lex-text :pos 0 :start 0 :input "hello" :tokens []})

  (collect-blocks (:tokens (tokenize* "Hello {{a|filter:1}} {% uppercase %}kids{% end-uppercase %}")))

  #_:end)
