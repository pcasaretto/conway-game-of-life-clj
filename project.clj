(defproject conway-game-of-life-clj "0.1.0-SNAPSHOT"
  :description "Game of Life in clojure"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [quil "4.3.1563"]
                 [org.clojure/tools.cli "1.1.230"]
                 [org.clojure/math.combinatorics "0.3.0"]]
  :main ^:skip-aot conway-game-of-life-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
