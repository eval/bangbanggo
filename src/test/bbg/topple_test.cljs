(ns bbg.topple-test
  (:require [bbg.topple :as sut]
            [bbg.util :refer [log warn error]]
            [cljs.test :refer [deftest is testing]]
            [matcher-combinators.config]))

(log "some info")

(warn "some warning")

(error "some error")

(deftest render-test
  (let [fut #(sut/render %1 %2)]
    (testing "simple template"
      (is (= "Some template" (fut "Some template" {})))
      (is (= "Some template" (fut "Some template" {:a 1})))
      (is (= "Hello"
             (fut "Hello{{world}}" {})))
      (is (= "HelloWorld"
             (fut "Hello{{world}}" {:world "World"})))
      (is (= "HelloWorld"
             (fut "Hello{{world}}" {"world" "World"}))))
    (testing "filters"
      (is (= "Capslocking: "
             (fut "Capslocking: {{hello|uppercase}}" {})))
      (is (= "Capslocking: HELLO"
             (fut "Capslocking: {{hello|uppercase}}" {:hello "hello"})))
      (is (= "Capslocking: EL!"
             (fut "Capslocking: {{hello|uppercase|subs:1:3}}!" {:hello "hello"}))))
    (testing "tags"
      (is (= "Capslocking: FOO"
             (fut "Capslocking: {% uppercase \"foo\"%}" {})))
      (is (= "Capslocking: FOO BAR"
             (fut "Capslocking: {% uppercase %}foo bar{%end-uppercase %}" {})))
      (is (= "Capslocking: GERT!"
             (fut "Capslocking: {% uppercase %}{{name}}!{% end-uppercase%}" {:name "gert"})))
      (let [tpl "{% if-matches #\"^@\" name%}Doing a direct lookup for {{name|subs:1}}{% else %}Searching for {{name}}{% end-if-matches %}"]
        (is (= "Doing a direct lookup for devise"
               (fut tpl {:name "@devise"})))
        (is (= "Searching for devise"
               (fut tpl {:name "devise"})))))))
