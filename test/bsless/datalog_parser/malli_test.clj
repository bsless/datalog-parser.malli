(ns bsless.datalog-parser.malli-test
  (:require [clojure.test :as t]
            [bsless.datalog-parser.malli :as sut :refer [parse]]))

(t/deftest conform
  (t/testing "find where"
    (t/is
     (= {:find-spec {:_ :find, :find [:find-rel [[:variable '?e]]]},
         :return-map? nil,
         :with-clause? nil,
         :inputs? nil,
         :where-clauses?
         [{:_ :where,
           :clauses
           [[:expression-clause
             [:data-pattern
              {:pattern [[:variable '?e] [:constant [:keyword :age]] [:constant [:number 42]]]
               :src-var? nil}]]]}]}
        (parse
         '[:find ?e
           :where [?e :age 42]]))))
  (t/testing "where blank"
    (t/is
     (= {:find-spec {:_ :find, :find [:find-rel [[:variable '?x]]]},
         :return-map? nil,
         :with-clause? nil,
         :inputs? nil,
         :where-clauses?
         [{:_ :where,
           :clauses
           [[:expression-clause
             [:data-pattern
              {:pattern [[:blank '_] [:constant [:keyword :likes]] [:variable '?x]]
               :src-var? nil}]]]}]}
        (parse
         '[:find ?x
           :where [_ :likes ?x]]))))
  (t/testing "src var"
    (parse
     '[:find ?release-name
       :in $
       :where [$ _ :release/name ?release-name]]))
  (t/testing "in"
    (parse
     '[:find ?release-name
       :in $ ?artist-name
       :where [?artist :artist/name ?artist-name]
       [?release :release/artists ?artist]
       [?release :release/name ?release-name]]))
  (t/testing "in pattern"
    (t/is
     (=
      {:find-spec {:_ :find, :find [:find-rel [[:variable '?release]]]},
       :return-map? nil
       :with-clause? nil
       :inputs?
       {:_ :in,
        :inputs
        [[:src-var '$]
         [:binding
          [:bind-tuple
           [[:variable '?artist-name] [:variable '?release-name]]]]]},
       :where-clauses?
       [{:_ :where,
         :clauses
         [[:expression-clause
           [:data-pattern
            {:pattern
             [[:variable '?artist]
              [:constant [:keyword :artist/name]]
              [:variable '?artist-name]]
             :src-var? nil}]]
          [:expression-clause
           [:data-pattern
            {:pattern
             [[:variable '?release]
              [:constant [:keyword :release/artists]]
              [:variable '?artist]]
             :src-var? nil}]]
          [:expression-clause
           [:data-pattern
            {:pattern
             [[:variable '?release]
              [:constant [:keyword :release/name]]
              [:variable '?release-name]]
             :src-var? nil}]]]}]}
      (parse
       '[:find ?release
         :in $ [?artist-name ?release-name]
         :where [?artist :artist/name ?artist-name]
         [?release :release/artists ?artist]
         [?release :release/name ?release-name]]))))

  (t/testing "find rel pattern"
    (t/is
     (= {:find-spec
         {:_ :find,
          :find
          [:find-rel [[:variable '?artist-name] [:variable '?release-name]]]},
         :return-map? nil,
         :with-clause? nil,
         :inputs? nil
         :where-clauses?
         [{:_ :where,
           :clauses
           [[:expression-clause
             [:data-pattern
              {:pattern
               [[:variable '?release]
                [:constant [:keyword :release/name]]
                [:variable '?release-name]]
               :src-var? nil}]]
            [:expression-clause
             [:data-pattern
              {:pattern
               [[:variable '?release]
                [:constant [:keyword :release/artists]]
                [:variable '?artist]]
               :src-var? nil}]]
            [:expression-clause
             [:data-pattern
              {:pattern
               [[:variable '?artist]
                [:constant [:keyword :artist/name]]
                [:variable '?artist-name]]
               :src-var? nil}]]]}]}
        (parse
         '[:find ?artist-name ?release-name
           :where [?release :release/name ?release-name]
           [?release :release/artists ?artist]
           [?artist :artist/name ?artist-name]]))))

  (t/testing "find coll ..."
    (t/is
     (= {:find-spec
         {:_ :find,
          :find [:find-coll {:find-elem [:variable '?release-name], :_ '...}]},
         :inputs?
         {:_ :in,
          :inputs [[:src-var '$] [:binding [:bind-scalar '?artist-name]]]},
         :where-clauses?
         [{:_ :where,
           :clauses
           [[:expression-clause
             [:data-pattern
              {:pattern
               [[:variable '?artist]
                [:constant [:keyword :artist/name]]
                [:variable '?artist-name]]}]]
            [:expression-clause
             [:data-pattern
              {:pattern
               [[:variable '?release]
                [:constant [:keyword :release/artists]]
                [:variable '?artist]]}]]
            [:expression-clause
             [:data-pattern
              {:pattern
               [[:variable '?release]
                [:constant [:keyword :release/name]]
                [:variable '?release-name]]}]]]}]}
        (parse
         '[:find [?release-name ...]
           :in $ ?artist-name
           :where [?artist :artist/name ?artist-name]
           [?release :release/artists ?artist]
           [?release :release/name ?release-name]]))))

  (t/testing "find tuple"
    (t/is
     (= '{:find-spec
          {:_ :find,
           :find
           [:find-tuple
            [[:variable ?year] [:variable ?month] [:variable ?day]]]},
          :inputs?
          {:_ :in, :inputs [[:src-var $] [:binding [:bind-scalar ?name]]]},
          :where-clauses?
          [{:_ :where,
            :clauses
            [[:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/name]
                 [:variable ?name]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/startDay]
                 [:variable ?day]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/startMonth]
                 [:variable ?month]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/startYear]
                 [:variable ?year]]}]]]}]}
        (parse
         '[:find [?year ?month ?day]
           :in $ ?name
           :where [?artist :artist/name ?name]
           [?artist :artist/startDay ?day]
           [?artist :artist/startMonth ?month]
           [?artist :artist/startYear ?year]] ))))

  (t/testing "find scalar"
    (t/is
     (= '{:find-spec
          {:_ :find, :find [:find-scalar {:find-elem [:variable ?year], :_ .}]},
          :inputs?
          {:_ :in, :inputs [[:src-var $] [:binding [:bind-scalar ?name]]]},
          :where-clauses?
          [{:_ :where,
            :clauses
            [[:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/name]
                 [:variable ?name]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/startYear]
                 [:variable ?year]]}]]]}]}
        (parse
         '[:find ?year .
           :in $ ?name
           :where [?artist :artist/name ?name]
           [?artist :artist/startYear ?year]]))))

  (t/testing "find scalar expression not"
    (t/is
     (= '{:find-spec
          {:_ :find,
           :find
           [:find-scalar
            {:find-elem
             [:aggregate
              {:aggregate-fn-name count, :fn-args [[:variable ?eid]]}],
             :_ .}]},
          :where-clauses?
          [{:_ :where,
            :clauses
            [[:expression-clause
              [:data-pattern
               {:pattern [[:variable ?eid] [:constant :artist/name]]}]]
             [:not-clause
              {:_ not,
               :clauses
               [[:expression-clause
                 [:data-pattern
                  {:pattern
                   [[:variable ?eid]
                    [:constant :artist/country]
                    [:constant :country/CA]]}]]]}]]}]}
        (parse
         '[:find (count ?eid) .
           :where
           [?eid :artist/name]
           (not [?eid :artist/country :country/CA])]))))


  (t/testing "not join"
    (t/is
     (= '{:find-spec
          {:_ :find,
           :find
           [:find-scalar
            {:find-elem
             [:aggregate
              {:aggregate-fn-name count, :fn-args [[:variable ?artist]]}],
             :_ .}]},
          :where-clauses?
          [{:_ :where,
            :clauses
            [[:expression-clause
              [:data-pattern
               {:pattern [[:variable ?artist] [:constant :artist/name]]}]]
             [:not-join-clause
              {:_ not-join,
               :variables [?artist],
               :clauses
               [[:expression-clause
                 [:data-pattern
                  {:pattern
                   [[:variable ?release]
                    [:constant :release/artists]
                    [:variable ?artist]]}]]
                [:expression-clause
                 [:data-pattern
                  {:pattern
                   [[:variable ?release]
                    [:constant :release/year]
                    [:constant 1970]]}]]]}]]}]}
        (parse
         '[:find (count ?artist) .
           :where [?artist :artist/name]
           (not-join [?artist]
                     [?release :release/artists ?artist]
                     [?release :release/year 1970])]))))

  (t/testing "pred expr"
    (t/is
     (= '{:find-spec
          {:_ :find, :find [:find-rel [[:variable ?name] [:variable ?year]]]},
          :where-clauses?
          [{:_ :where,
            :clauses
            [[:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/name]
                 [:variable ?name]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/startYear]
                 [:variable ?year]]}]]
             [:expression-clause
              [:pred-expr
               {:expr
                {:pred <, :fn-args [[:variable ?year] [:constant 1600]]}}]]]}]}
        (parse
         '[:find ?name ?year
           :where [?artist :artist/name ?name]
           [?artist :artist/startYear ?year]
           [(< ?year 1600)]]))))

  (t/testing "fn expr with bindings"
    (t/is
     (= '{:find-spec
          {:_ :find,
           :find [:find-rel [[:variable ?track-name] [:variable ?minutes]]]},
          :inputs?
          {:_ :in,
           :inputs [[:src-var $] [:binding [:bind-scalar ?artist-name]]]},
          :where-clauses?
          [{:_ :where,
            :clauses
            [[:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?artist]
                 [:constant :artist/name]
                 [:variable ?artist-name]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?track]
                 [:constant :track/artists]
                 [:variable ?artist]]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?track]
                 [:constant :track/duration]
                 [:variable ?millis]]}]]
             [:expression-clause
              [:fn-expr
               {:expr
                {:fn quot, :fn-args [[:variable ?millis] [:constant 60000]]},
                :binding [:bind-scalar ?minutes]}]]
             [:expression-clause
              [:data-pattern
               {:pattern
                [[:variable ?track]
                 [:constant :track/name]
                 [:variable ?track-name]]}]]]}]}
        (parse
         '[:find ?track-name ?minutes
           :in $ ?artist-name
           :where [?artist :artist/name ?artist-name]
           [?track :track/artists ?artist]
           [?track :track/duration ?millis]
           [(quot ?millis 60000) ?minutes]
           [?track :track/name ?track-name]]))))

  (t/testing "pull expr"
    (t/is
     (= [[:attr-name :artist/name]
         [:attr-expr
          {:attr-name :track/_artists,
           :attr-options [[:limit-expr {:_ :limit, :limit 10}]]}]]
        (parse '[:artist/name (:track/_artists :limit 10)]))))

  (t/testing "pull map"
    (t/is
     (= '[[:attr-name :track/name]
          [:map-spec
           {[:attr-name :track/artists]
            [:pull-pattern [[:attr-name :db/id] [:attr-name :artist/name]]]}]]
        (parse '[:track/name {:track/artists [:db/id :artist/name]}])))))

(comment
  (parse
   '[:find ?e
     :where [?e :age 42]])

  (parse
   '[:find ?track-name ?minutes
     :in $ ?artist-name
     :where [?artist :artist/name ?artist-name]
     [?track :track/artists ?artist]
     [?track :track/duration ?millis]
     [(quot ?millis 60000) ?minutes]
     [?track :track/name ?track-name]])

  (parse
   '[:find (count ?artist) .
     :where [?artist :artist/name]
     (not-join [?artist]
               [?release :release/artists ?artist]
               [?release :release/year 1970])])

  (parse
   '[:find ?e
     :keys foo
     :in $ ?fname ?lname
     :where [?e :user/firstName ?fname]
     [?e :user/lastName ?lname]]))


(parse
 '[:find [?release-name ...]
   :in $ ?artist-name
   ;; :where
   ;; [?artist :artist/name ?artist-name]
   ;; [?release :release/artists ?artist]
   ;; [?release :release/name ?release-name]
   ])
