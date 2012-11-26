(defproject fossa "0.1.0-SNAPSHOT"
  :description "Chewing through GBIF species occurrence data like it eats lemurs, fossa (cryptoprocta ferox) is a cat-like carnivorous mammal from Madagascar"
  :url "http://en.wikipedia.org/wiki/Fossa_(animal)"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :resources-path "resources"
  :profiles {:dev {:dependencies [[org.apache.hadoop/hadoop-core "0.20.2-dev"]
                                  [midje-cascalog "0.4.0"]]}
             :plugins [[lein-midje "2.0.0-SNAPSHOT"]]}
  :jvm-opts ["-XX:MaxPermSize=128M"
             "-XX:+UseConcMarkSweepGC"
             "-Xms1024M" "-Xmx1800M" "-server"]
  :plugins [[lein-swank "1.4.4"]
            [lein-emr "0.1.0-SNAPSHOT"]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [cascalog "1.9.0"]
                 [backtype/dfs-datastores "1.1.3"]
                 [backtype/dfs-datastores-cascading "1.2.0"]
                 [cljts "0.2.0"]])
