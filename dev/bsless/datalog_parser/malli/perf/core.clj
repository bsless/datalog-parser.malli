(ns bsless.datalog-parser.malli.perf.core
  (:require
   [clj-async-profiler.core :as prof]
   [bsless.datalog-parser.spec :as dps]
   [criterium.core :as cc]
   [clojure.spec.alpha :as s]
   [bsless.datalog-parser.malli :refer [parse]]))

(def q1
  '[:find (count ?artist) .
    :where [?artist :artist/name]
    (not-join [?artist]
              [?release :release/artists ?artist]
              [?release :release/year 1970])])

(cc/quick-bench (parse q1))

(cc/quick-bench (clojure.spec.alpha/conform ::dps/query q1))

(def q2 '[:find ?e
         :keys foo
         :in $ ?fname ?lname
         :where [?e :user/firstName ?fname]
         [?e :user/lastName ?lname]])

(cc/quick-bench (parse q2))

(prof/serve-files 7777)

(prof/profile
 (time
  (dotimes [_ 1e6]
    (parse q2))))
