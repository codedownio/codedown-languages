;; Variable inspector for the Clojure (clojupyter) Jupyter kernel.
;;
;; Defines a few helper vars whose names start with "codedown-variable-inspector"
;; so they're easy to filter out of the listing. cheshire is on clojupyter's
;; classpath, so we use it for JSON. The frontend runs this once at startup, then
;; calls (codedown-variable-inspector-list) and
;; (codedown-variable-inspector-inspect "name").

(require '[cheshire.core :as codedown-variable-inspector-json])

(defn codedown-variable-inspector-info [v]
  (let [val (try (deref v) (catch Throwable _ nil))
        s (try (pr-str val) (catch Throwable _ "<unprintable>"))]
    {:type (some-> val class .getName)
     :size nil
     :shape (when (counted? val) [(count val)])
     :content (if (> (count s) 150) (str (subs s 0 150) " ...") s)
     :isMatrix (boolean (or (sequential? val) (map? val) (set? val)))}))

(defn codedown-variable-inspector-list []
  (println
    (codedown-variable-inspector-json/generate-string
      (into {}
        (for [[sym v] (ns-interns *ns*)
              :let [n (name sym)]
              :when (not (.startsWith n "codedown-variable-inspector"))]
          [n (codedown-variable-inspector-info v)])))))

(defn codedown-variable-inspector-inspect [var-name]
  (let [v (ns-resolve *ns* (symbol var-name))]
    (if (nil? v)
      (println (codedown-variable-inspector-json/generate-string
                 {:name var-name :isMatrix false :content "<undefined>"}))
      (let [val (try (deref v) (catch Throwable _ nil))
            ismat (boolean (or (sequential? val) (map? val) (set? val)))
            table (cond
                    (map? val) {:columns ["key" "value"] :data (mapv (fn [[k vv]] [k vv]) val)}
                    (or (sequential? val) (set? val)) {:columns ["value"] :data (mapv (fn [x] [x]) val)}
                    :else nil)]
        (println (codedown-variable-inspector-json/generate-string
                   {:name var-name
                    :type (some-> val class .getName)
                    :size nil
                    :shape (when (counted? val) [(count val)])
                    :isMatrix ismat
                    :content (pr-str val)
                    :table table}))))))
