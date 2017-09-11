(ns logical-interpreter)

(require '[clojure.string :as str])

(def dbok ").")

(defn trim [string]
	 (str/trim string)
)

(defn index [string val]
	(str/index-of string val)
)

;Fin definiciones generales


(defn is-ok-query
     [query]
  (boolean(re-find #"[a-zA-Z0-9_]*[\(][a-zA-Z0-9_, ]*[\)]" query))
 )

(defn is-ok-database
     [database]
  (str/ends-with? (str/trim database)  dbok) 
 )

(defn is-rule [text]
  (str/includes? (trim text) ":-")
)


(defn parser-fact
  [lineas]
	(let [fkey (trim (subs lineas 0 (index lineas "(")))
		fargs (trim (subs lineas (+ (index lineas "(") 1) (index lineas ")")))]
		(let [args (map trim (str/split fargs #","))]
		     [fkey args]
    )
  )
)

(defn parser-rule
  [lineas]
  (let [rkey (trim (subs lineas 0 (index lineas ":")))
		rargs (trim (subs lineas (+ (index lineas "-") 1) (index lineas ".")))]
		     [rkey rargs]
  )
)

(defn parser-database
  [string]
  (if (is-rule string)
        (parser-rule string)
        (parser-fact string)
  ) 
)


(defn rules-to-map [rule]
  (let [rkey  (subs (first rule) 0 (index (first rule) "("))
		rargs (subs (first rule) (+ (index (first rule) "(") 1) (index (first rule) ")"))]
		(let [args (map trim (str/split rargs #","))]
		     (hash-map (keyword rkey) [args (second rule)])
    )
  )
)

(defn exec-fact [query facts]
  (boolean (some #(= % query) facts))
)

(defn replace-val [val [k v]]
  (str/replace val k v)
)

(defn exec-rule [query rules facts]

  ;Merge rules (database) with rules-facts (query)
   (def merge-rules (zipmap (first rules) (second query)))
  
  ;Split multiples facts in a rule
   (def facts-rule  (map trim (str/split (second rules) #"\%,")))
 
  
  ;Replace all keys-query, with value rule-facts
   (def value (for [argument (map parser-fact 
            (for [value facts-rule](reduce replace-val value merge-rules))
                   )] (exec-fact argument facts)))
  
  (every? true? value)
  
)

(defn exec-query [query facts rules]

  (if (contains? rules (keyword(first query)))
      (exec-rule query (rules (keyword(first query))) facts)
      (exec-fact query facts)
    )
)

(defn custom-database [database]
  (remove empty? (str/split (str/replace database ")," ")%,") #"\n"))
)

(defn get-facts [list]
 (filter (fn [[k v]] (not (str/includes? k ","))) list)
)

(defn get-rules [list]
 (filter (fn [[k v]] (str/includes? k ",")) list)
)

(defn evaluate-query [database query] 
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"

  (if (and (is-ok-database database) (is-ok-query query))
      (do
        (def full-list (map parser-database (custom-database database)))
	
	(def facts (get-facts full-list))
	(def rules (get-rules full-list))

        (def map-rules (reduce merge (map rules-to-map rules)))

        (exec-query (parser-fact query) facts map-rules)
      )
      nil
      )
)
 
