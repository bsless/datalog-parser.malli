(ns bsless.datalog-parser.malli
  (:require
   [malli.core :as m]
   [clojure.walk :as walk]))

(defn -plain-symbol?
  [x]
  (let [% (. ^clojure.lang.Named x (getName))
        ch (.charAt ^String % (unchecked-int 0))]
    (if (= \$ ch) false (if (= ch \?) false true))))

(defn -var-symbol?
  [x]
  (let [% (. ^clojure.lang.Named x (getName))]
    (= \? (.charAt ^String % (unchecked-int 0)))))

(defn -src-symbol?
  [x]
  (let [% (. ^clojure.lang.Named x (getName))]
    (= \$ (.charAt ^String % (unchecked-int 0)))))

(def reg
  {

;;; query = [find-spec return-map-spec? with-clause? inputs? where-clauses?]
   ::query [:catn
            [:find-spec ::find-spec]
            [:return-map? ::return-map?]
            [:with-clause? ::with-clause?]
            [:inputs? ::inputs?]
            [:where-clauses? ::where-clauses?]],

;;; find-spec = ':find' (find-rel | find-coll | find-tuple | find-scalar)
   ::find-spec [:catn
                [:_ [:= :find]]
                [:find [:altn
                        [:find-rel ::find-rel]
                        [:find-coll ::find-coll]
                        [:find-tuple ::find-tuple]
                        [:find-scalar ::find-scalar]]]],

;;; return-map = (return-keys | return-syms | return-strs)
   ::return-map? [:? ::return-map],
   ::return-map [:altn
                 [:return-keys ::return-keys]
                 [:return-syms ::return-syms]
                 [:return-strs ::return-strs]],

;;; find-rel = find-elem+
   ::find-rel [:+ ::find-elem],

;;; find-coll = [find-elem '...']
   ::find-coll [:schema [:catn
                         [:find-elem ::find-elem]
                         [:_ [:= '...]]],]

;;; find-scalar = find-elem '.'
   ::find-scalar [:catn [:find-elem ::find-elem] [:_ [:= '.]]],

;;; find-tuple = [find-elem+]
   ::find-tuple [:+ ::find-elem],

;;; find-elem                  = (variable | pull-expr | aggregate)
   ::find-elem [:altn
                [:variable ::variable]
                [:pull-expr ::pull-expr]
                [:aggregate ::aggregate]],

;;; return-keys = ':keys' symbol+
   ::return-keys [:catn [:_ [:= :keys]] [:return [:+ symbol?]]],

;;; return-syms = ':syms' symbol+
   ::return-syms [:catn [:_ [:= :syms]] [:return [:+ symbol?]]],

;;; return-strs = ':strs' symbol+
   ::return-strs [:catn [:_ [:= :strs]] [:return [:+ symbol?]]],


;;; pull-expr = ['pull' variable pattern]
   ::pull-expr [:catn
                [:_ [:= 'pull]]
                [:variable ::variable]
                [:pull-pattern ::pull-pattern]],

;;; pattern = (pattern-name | pattern-data-literal)
   ::pattern [:altn
              [:pattern-name ::pattern-name]
              [:pattern-data-literal ::pattern-data-literal]],

;;; aggregate = [aggregate-fn-name fn-arg+]

   ::aggregate-fn-name [:enum
                        'max 'sum 'min 'stddev 'variance 'count 'rand
                        'sample 'median 'count-distinct 'avg 'distinct],

   ::aggregate [:schema [:catn
                         [:aggregate-fn-name ::aggregate-fn-name]
                         [:fn-args ::fn-args]],]

   ::fn-args [:+ ::fn-arg],

;;; fn-arg = (variable | constant | src-var)
   ::fn-arg [:altn
             [:variable ::variable]
             [:constant ::constant]
             [:src-var ::src-var]],


;;; with-clause = ':with' variable+
   ::with-clause? [:? ::with-clause],
   ::with-clause [:catn [:_ [:= :with]] [:variables ::variables]],

;;; where-clauses = ':where' clause+
   ::where-clauses? [:? ::where-clauses],
   ::where-clauses [:+ ::where-clause],
   ::where-clause [:catn [:_ [:= :where]] [:clauses ::clauses]],

   ::clauses [:+ ::clause],

;;; inputs = ':in' (src-var | binding | pattern-name | rules-var)+

   ::inputs? [:? ::inputs],

   ::inputs [:catn
             [:_ [:= :in]]
             [:inputs
              [:+
               [:altn
                [:src-var ::src-var]
                [:binding ::binding]
                [:pattern-name ::pattern-name]
                [:rules-var ::rules-var]]]]],

;;; src-var = symbol starting with "$"
   ::src-var [:and simple-symbol? [:fn -src-symbol?]],
   ::src-var? [:? ::src-var],

;;; variable = symbol starting with "?"
   ::variable [:and simple-symbol? [:fn -var-symbol?]],
   ::variables [:+ ::variable],

;;; rules-var = the symbol "%"
   ::rules-var [:= '%],

;;; plain-symbol = symbol that does not begin with "$" or "?"
   ::plain-symbol [:and simple-symbol? [:fn -plain-symbol?]],

;;; pattern-name = plain-symbol
   ::pattern-name ::plain-symbol,

;;; and-clause = [ 'and' clause+ ]
   ::and-clause [:catn [:_ [:= 'and]] [:clauses ::clauses]],

;;; expression-clause = (data-pattern | pred-expr | fn-expr | rule-expr)
   ::expression-clause [:orn
                        [:data-pattern ::data-pattern]
                        [:pred-expr ::pred-expr]
                        [:fn-expr ::fn-expr]
                        [:rule-expr [:ref ::rule-expr]]],

;;; rule-expr = [ src-var? rule-name (variable | constant | '_')+]
   ::rule-expr [:catn
                [:src-var? ::src-var?]
                [:rule-name ::rule-name]
                [:args [:+ [:orn
                            [:variable [:ref ::variable]]
                            [:constant [:ref ::constant]]
                            [:blank ::blank]]]]],

;;; not-clause = [ src-var? 'not' clause+ ]
   ::not-clause [:catn
                 [:src-var? ::src-var?]
                 [:_ [:= 'not]]
                 [:clauses ::clauses]],

;;; not-join-clause = [ src-var? 'not-join' [variable+] clause+ ]
   ::not-join-clause [:catn
                      [:src-var? ::src-var?]
                      [:_ [:= 'not-join]]
                      [:variables [:schema [:+ ::variable]]]
                      [:clauses ::clauses]],

;;; or-clause = [ src-var? 'or' (clause | and-clause)+]
   ::or-clause [:catn [:src-var? ::src-var?]
                [:_ [:= 'or]]
                [:clauses
                 [:+
                  [:orn
                   [:clause [:ref ::clause]]
                   [:and-clause [:ref ::and-clause]]]]]],

;;; or-join-clause = [ src-var? 'or-join' rule-vars (clause | and-clause)+ ]
   ::or-join-clause [:catn
                     [:src-var? ::src-var?]
                     [:_ [:= 'or-join]]
                     [:rule-vars ::rule-vars]
                     [:clauses [:+
                                [:orn
                                 [:clause [:ref ::clause]]
                                 [:and-clause [:ref ::and-clause]]]]]],

;;; rule-vars = [variable+ | ([variable+] variable*)]
   ::rule-vars [:altn
                [:simple [:+ ::variable]]
                [:nested
                 [:schema
                  [:catn
                   [:nested
                    [:schema
                     [:+ ::variable]]]
                   [:more [:* ::variable]]]]]],

;;; clause                     = (not-clause | not-join-clause | or-clause | or-join-clause | expression-clause)
   ::clause [:orn
             [:not-clause [:ref ::not-clause]]
             [:not-join-clause [:ref ::not-join-clause]]
             [:or-clause [:ref ::or-clause]]
             [:or-join-clause [:ref ::or-join-clause]]
             [:expression-clause ::expression-clause]],

;;; data-pattern = [ src-var? (variable | constant | '_')+ ]
   ::data-pattern [:catn
                   [:src-var? ::src-var?]
                   [:pattern
                    [:+
                     [:altn
                      [:variable ::variable]
                      [:constant ::constant]
                      [:blank ::blank]]]]],

;;; constant = any non-variable data literal
   ::constant [:orn
               [:number number?]
               [:string string?]
               [:boolean boolean?]
               [:keyword keyword?]
               [:set [:set [:ref ::constant]]]
               [:vector [:vector [:ref ::constant]]]
               [:list [:sequential [:ref ::constant]]]
               [:map [:map-of [:ref ::constant] [:ref ::constant]]]],

;;; pred-expr = [ [pred fn-arg+] ]
   ::pred-expr [:catn [:expr [:schema [:catn [:pred ::pred] [:fn-args ::fn-args]]]]],

   ::pred [:enum '<= '= '>= 'odd? '< '> 'even?],

;;; fn-expr = [ [fn fn-arg+] binding]
   ::fn-expr [:catn
              [:expr [:schema [:catn [:fn ::fn] [:fn-args ::fn-args]]]]
              [:binding ::binding]],

   ::fn (->> 'clojure.core
             ns-publics
             (filter (fn [[_ v]] (let [m (meta v)] (and (not (:macro m)) (:arglists m)))))
             (map key)
             (into [:enum])),

;;; binding = (bind-scalar | bind-tuple | bind-coll | bind-rel)
   ::binding [:altn
              [:bind-scalar ::bind-scalar]
              [:bind-tuple ::bind-tuple]
              [:bind-coll ::bind-coll]
              [:bind-rel ::bind-rel]],

;;; bind-scalar = variable
   ::bind-scalar ::variable,

;;; bind-tuple = [ (variable | '_')+]
   ::bind-tuple [:+
                 [:altn
                  [:variable ::variable]
                  [:blank ::blank]]],

;;; bind-coll = [variable '...']
   ::bind-coll [:catn [:variable ::variable] [:_ [:= '...]]],

;;; bind-rel = [ [(variable | '_')+] ]
   ::bind-rel [:tuple
               [:+
                [:altn
                 [:variable ::variable]
                 [:blank ::blank]]]],

;;; rule = [ [rule-head clause+]+ ]
   ::rule [:+
           [:schema
            [:catn
             [:rule-head ::rule-head]
             [:clauses ::clauses]]]],

;;; rule-head = [rule-name rule-vars]
   ::rule-head [:catn
                [:rule-name ::rule-name]
                [:rule-vars ::rule-vars]],

;;; rule-name = unqualified plain-symbol
   ::rule-name ::plain-symbol,

   ::blank [:= '_],

;;; pattern = [attr-spec+]
   ::pull-pattern [:+ ::attr-spec],

;;; attr-spec = attr-name | wildcard | map-spec | attr-expr
   ::attr-spec [:orn
                [:attr-name ::attr-name]
                [:wildcard ::wildcard]
                [:map-spec ::map-spec]
                [:attr-expr ::attr-expr]],

;;; attr-name = an edn keyword that names an attr
   ::attr-name :keyword,

;;; map-spec = { ((attr-name | attr-expr) (pattern | recursion-limit))+ }
   ::map-spec [:map-of
               [:orn
                [:attr-name [:ref ::attr-name]]
                [:attr-expr [:ref ::attr-expr]]]
               [:orn
                [:pull-pattern [:ref ::pull-pattern]]
                [:recursion-limit [:ref ::recursion-limit]]]],

;;; attr-expr = [attr-name attr-option+] | legacy-attr-expr
   ::attr-expr [:catn
                [:attr-name ::attr-name]
                [:attr-options [:* ::attr-option]]],

;;; attr-option = as-expr | limit-expr | default-expr
   ::attr-option [:altn
                  [:as-expr ::as-expr]
                  [:limit-expr ::limit-expr]
                  [:default-expr ::default-expr]]

;;; as-expr = [:as any-value]
   ::as-expr [:catn [:_ [:= :as]] [:value any?]]

;;; limit-expr = [:limit (positive-number | nil)]
   ::limit-expr [:catn [:_ [:= :limit]] [:limit [:maybe pos-int?]]],

;;; default-expr = [:default any-value]
   ::default-expr [:catn [:_ [:= :default]] [:default any?]],

;;; wildcard = "*" or '*'
   ::wildcard [:enum "*" '*],

;;; recursion-limit = positive-number | '...'
   ::recursion-limit [:orn [:number pos-int?] [:none [:= '...]]],
   ,})

(comment
  (require '[ubergraph.core :as g])
  (def og (atom (g/graph)))
  (doseq [[k' v] reg]
    (walk/postwalk
     (fn [k]
       (when (and (keyword? k) (get reg k))
         (swap! og g/add-directed-edges [(keyword (name k')) (keyword (name k))])))
     v))
  (g/pprint @og)
  (g/viz-graph @og)

  )

(defn schema
  ([] (schema ::query))
  ([?schema]
   (cond
     (keyword? ?schema) (-> [:schema {:registry reg} ?schema]
                            (m/schema {::m/lazy-entries true})))))

(defn parser
  ([] (parser ::query))
  ([?schema] (-> ?schema schema (m/parser {::m/lazy-entries true}))))

(defn explainer
  ([] (explainer ::query))
  ([?schema] (-> ?schema schema (m/explainer {::m/lazy-entries true}))))

(def parse (parser))
