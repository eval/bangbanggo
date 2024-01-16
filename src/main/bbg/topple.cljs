(ns bbg.topple
  "topple is a templating library that expands templates like selmer and nunjucks (i.e. just enough to support bbg's bang-templates).

  It supports the following filters:
  - uppercase
  - urlencode
  - subs (with args: e.g. `name|subs:1:2`)

  It supports the following tags:
  - uppercase ?literal-or-var
  - if-matches regex literal-or-var

  Example:
  (topple/render \"Hello, {{name|uppercase}}!
    {% if-matches #\\\"^(?i)world+$\\\" name %}Classic!{% else %}Beautiful name :){% end-if-matches %}\" {:name \"World\"})

  Note: The reason we can't use nunjucks is that it uses eval which is not allowed with the default mv3 CSP."
  (:require [bbg.topple.tokenizer :as tokenizer]
            [clojure.string :as string]
            [goog.string :as gstr]))

(def filters {"uppercase" #'string/upper-case
              "urlencode" #(gstr/urlEncode %)
              "subs"      subs})

(defn- resolve-block-arg [v args]
  (if (symbol? v) (get args (name v)) v))

(defn- split-items-by-type [type items]
  (let [[lhs [divider & rhs]] (split-with (comp (complement #{type}) :t) items)]
    [lhs divider rhs]))

(defn- render-items-dispatch [[{:keys [t] :as item} :as _items] _ctx]
  (let [item-type (cond-> t
                    (= t :block-fn) (vector (:v item)))
        aliases   {:left-meta               :placeholder
                   [:block-fn "uppercase"]  :uppercase-block
                   [:block-fn "if-matches"] :if-matches-block}]
    (get aliases item-type item-type)))

(defmulti render-items #'render-items-dispatch)

(defmethod render-items :text [[item :as items] ctx]
  (-> ctx
      (update :rendered str (:v item))
      (assoc :items (rest items))))

(defmethod render-items :var [[{var-name :v} :as items] {:keys [args] :as ctx}]
  (-> ctx
      (update :rendered str (get args var-name))
      (assoc :items (rest items))))

(defmethod render-items :filter [[{filter-name :v filter-args :filter/args} :as items] ctx]
  (let [filter-fn (get filters filter-name)]
    (-> ctx
        (update :rendered #(apply filter-fn % filter-args))
        (assoc :items (rest items)))))

(declare render*)

(defmethod render-items :placeholder [items {:keys [args] :as ctx}]
  (let [[[_left-meta & ph-items] _right-meta rem-items] (split-items-by-type :right-meta items)
        local-rendered                                  (render* ph-items args)]
    (-> ctx
        (update :rendered str local-rendered)
        (assoc :items rem-items))))

(defmethod render-items :if-matches-block [[item & rem-items] {ctx-args :args :as ctx}]
  (let [{[regex v]             :block/args
         [if-items else-items] :block/items} item
        v                                    (resolve-block-arg v ctx-args)
        to-render-items                      (if (re-find regex v) if-items else-items)
        local-rendered                       (render* to-render-items ctx-args)]
    (-> ctx
        (update :rendered str local-rendered)
        (assoc :items rem-items))))

(defmethod render-items :uppercase-block [[item & rem-items] {ctx-args :args :as ctx}]
  (let [{[arg]   :block/args
         [items] :block/items} item
        local-rendered         (when-let [v (if arg
                                              (resolve-block-arg arg ctx-args)
                                              (render* items ctx-args))]
                                 (string/upper-case v))]
    (-> ctx
        (update :rendered str local-rendered)
        (assoc :items rem-items))))


;; unknown item - just consume it
(defmethod render-items :default [[_item & rem-items] ctx]
  (assoc ctx :items rem-items))


(defn render* [items args]
  (let [args (update-keys args name)]
    (loop [{:keys [items rendered] :as ctx} {:rendered "" :args args :items items}]
      (if-not (seq items)
        rendered
        (recur (render-items items ctx))))))


(defn render
  "Renders template.
  `(render \"Hello {{s}}!\" {:s \"@devise\"})`"
  [tpl args]
  (render* (tokenizer/tokenize tpl) args))

(comment

  (render "Hello, {{name|uppercase}}!\n {% if-matches #\"(?i)world\" name %}(Classic! ;){% else %}(Nice name! ;){% end-if-matches %}" {:name "Gert"})
  #_:end)
