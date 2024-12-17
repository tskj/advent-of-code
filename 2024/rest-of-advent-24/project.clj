(defproject rest-of-advent-24 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.1.0"] 
                 [net.clojars.john/injest "0.1.0-beta.8"]]
  :plugins [[cider/cider-nrepl "0.29.0"]]
  :profiles {:kaocha {:dependencies [[lambdaisland/kaocha "1.91.1392"]]}}
  :aliases {"kaocha" ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"]
            "test-runner" ["with-profile" "+dev" "run" "-m" "kaocha.runner"]}
  :repl-options {:init-ns rest-of-advent-24.core}
  :test-paths ["test"])
