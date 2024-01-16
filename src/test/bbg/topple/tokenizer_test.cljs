(ns bbg.topple.tokenizer-test
  (:require [bbg.topple.tokenizer :as sut]
            [cljs.test :refer [deftest is testing]]
            [matcher-combinators.config]
            [matcher-combinators.matchers :as m2]
            [matcher-combinators.test :refer [match?]]))

(matcher-combinators.config/disable-ansi-color!)

(deftest next-char-test
  (let [fut (fn fut
              ([pos]
               (#'sut/next-char {:input "123" :pos pos}))
              ([pos n]
               (#'sut/next-char {:input "123" :pos pos} n)))]
    (testing "Yields next char based on :pos"
      (is (= "1" (fut 0)))
      (is (= "3" (fut 2))))

    (testing "Allows for multiple chars"
      (is (= "12" (fut 0 2)))
      (is (= "23" (fut 1 2))))

    (testing "Yields :eos when no more input"
      (is (= :eos (fut 3)))
      (is (= :eos (fut 2 2))))))

(deftest read-char-test
  (let [pos-after-reading-n #(:pos (#'sut/read-char {:input %1 :pos 0} %2))]
    (testing "it advances :pos by n"
      (is (= 1 (pos-after-reading-n "123" 1)))
      (is (= 2 (pos-after-reading-n "123" 2))))

    (testing ":pos never goes beyond the length of the input"
      (is (= 3 (pos-after-reading-n "123" 10))))))

(deftest emit-token-test
  (let [emitted-token #(-> {:input %1 :start %2 :pos %3}
                           (#'sut/emit-token %4)
                           :tokens
                           last)]
    (testing "Adds a token based on :input, :start and :pos"
      (is (match? {:t :foo :v "1"} (emitted-token "123" 0 1 :foo)))
      (is (match? {:t :foo :v "12"} (emitted-token "123" 0 2 :foo))))))

(deftest tokenize-test
  (testing "tokens"
    (let [tokenize-tokens #(:tokens (sut/tokenize* %))]
      (testing "text only"
        (is (match? (m2/seq-of {:v seq}) (tokenize-tokens "hello"))
            "contains no tokens with an empty :v")
        (is (match? [{:v "hello"}]
                    (tokenize-tokens "hello")))
        (is (empty? (tokenize-tokens ""))))

      (testing "vars"
        (is (match? (m2/seq-of {:v seq}) (tokenize-tokens "{{hello}}"))
            "contains no tokens with an empty :v")
        (is (match? (m2/embeds [{:t :var :v "q"}]) (tokenize-tokens "hello {{q}}"))
            "contains the var-token"))

      (testing "filter"
        (is (match? (m2/seq-of {:v seq}) (tokenize-tokens "{{hello|some-filter|more}}"))
            "contains no tokens with an empty :v")
        (is (match? (m2/embeds [{:t :filter :v "filter"}]) (tokenize-tokens "hello {{q|filter}}"))
            "contains the filter-token")
        (is (match? (m2/embeds [{:t :filter :v "filter"} {:t :filter :v "moar"}])
                    (tokenize-tokens "hello {{q|filter|moar}}"))
            "contains the filter-tokens")
        (testing "args"
          (is (match? (m2/seq-of {:v seq}) (tokenize-tokens "{{hello|a:1:2}}"))
              "contains no tokens with an empty :v")
          (is (match? (m2/embeds [{:t :filter-arg :v "1"} {:t :filter-arg :v "23"}])
                      (tokenize-tokens "hello {{q|filter:1:23}}"))
              "reads integers")
          (is (match? (m2/embeds [{:t :filter-arg :v "\"foo\""}])
                      (tokenize-tokens "hello {{q|filter:\"foo\"}}"))
              "reads strings")
          (is (match? (m2/embeds [{:t :filter-arg :v "\"quoting:\\\"EASY!\\\"\""}])
                      (tokenize-tokens "hello {{q|filter:\"quoting:\\\"EASY!\\\"\":1}}"))
              "reads strings with quotes"))))))

(deftest collect-blocks-test
  (let [fut #(sut/semantify %)]
    (testing "tag with arg"
      (comment (sut/tokenize "{% uppercase \"foo\" %}{% end-uppercase %}"))
      (let [tokens [{:start 3, :t :block-fn, :v "uppercase", :block/type :start}
                    {:start 13,
                     :t :block-fn-arg,
                     :v "\"foo\"",
                     :v/parsed "foo",
                     :v/type :string}]]
        (is (match? (m2/embeds [{:v "uppercase" :block/end 3 :block/start 3 :block/args ["foo"]}])
                    (fut tokens))
            "adds an item with block :start, :end and :args")))
    (testing "tag with block"
      (comment (sut/tokenize* "{% uppercase %}Foo{% end-uppercase %}"))
      (let [tokens [{:start 3, :t :block-fn, :v "uppercase", :block/type :start}
                    {:start 15, :t :text, :v "Foo"}
                    {:start 21, :t :block-fn, :v "end-uppercase", :block/type :end}]]
        (is (match? (m2/embeds [{:v "uppercase"
                                 :block/start 3
                                 :block/end 21
                                 :block/items [[map? map?]]}])
                    (fut tokens))
            "adds an item with block :start, :end and :args")
        #_(fut tokens)))
    (testing "tag with else-block"
      (comment (sut/tokenize* "{% uppercase %}Foo{% else %}{% end-uppercase %}"))
      (let [tokens [#_{:start 0, :t :left-teta, :v "{%"}
                    {:start 3, :t :block-fn, :v "uppercase", :block/type :start}
                    #_{:start 13, :t :right-teta, :v "%}"}
                    {:start 15, :t :text, :v "Foo"}
                    #_{:start 18, :t :left-teta, :v "{%"}
                    {:start 21, :t :block-fn, :v "else", :block/type :else}
                    #_{:start 26, :t :right-teta, :v "%}"}
                    #_{:start 28, :t :left-teta, :v "{%"}
                    {:start 31, :t :block-fn, :v "end-uppercase", :block/type :end}
                    #_{:start 45, :t :right-teta, :v "%}"}]]
        (is (match? (m2/embeds [{:v "uppercase"
                                 :block/start 3
                                 :block/end 31
                                 :block/items [[map?] [map? map?]]}])
                    (fut tokens))
            "adds an item with block :start, :end and :args")))))

(comment

  #_:end)
