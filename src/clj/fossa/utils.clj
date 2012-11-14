(ns fossa.utils)

(defn split-line
  "Returns vector of line values by splitting on tab."
  [line]
  (vec (.split line "\t")))

(defn mk-latlon-str
  "Make a comma-separated latlon string given lat and lon"
  [lat lon]
  (str lat "," lon))

(defn mk-latlon-map
  "Make a map with a latlon string as key and a value of [] (if no value
   supplied) or [val]"
  ([lat lon]
     {(mk-latlon-str lat lon) []})
  ([lat lon val]
     {(mk-latlon-str lat lon) [val]}))

(defn mk-empty-latlon-map
  "Makes a latlon map with empty vector as value"
  [lats lons]
  (apply merge (map mk-latlon-map lats lons)))

(defn collect-latlon-vals->map
  "Creates a map with latlon strings as keys. For values, if there are multiple
   entries for a given latlon, concatenates the entries together in a list.

   Usage:
     (collect-latlon-vals->map [1 2 1] [3 4 3] [10 11 12])
     ;=> {\"2,4\" (11), \"1,3\" (10 12)}"
  ([lats lons values]
     (let [merge-fn (comp vec concat)
           empty-map (mk-empty-latlon-map lats lons)
           ms (map mk-latlon-map lats lons values)]
       (apply merge-with merge-fn empty-map ms))))

(defn latlon->coord-str
  "Given a latitude and longitude, returns a WKT-formatted \"(lon lat)\"
   string."
  ([lat lon]
     (let [coord-str "%s %s"]
       (format coord-str lon lat)))
  ([[lat lon]]
     (latlon->coord-str lat lon)))

(defn latlons->wkt-multi-point
  "Returns WKT MULTIPOINT string from supplied lats and lons."
  [lats lons]
  (let [out-str "MULTIPOINT (%s)"
        sep ", "]
    (->> (map vector lats lons)
         (vec)
         (map latlon->coord-str)
         (interpose sep)
         (apply str)
         (format out-str))))

(defn mk-sorted-map
  "Make a map a sorted map."
  [m]
  (into (sorted-map) m))

(defn parse-for-wkt
  "Parse latlons for well known text, ensuring that order is retained
   using an ordered map, as for other data fields.

   Use of maps also ensures all unique latlons in output."
  [lats lons]
  (let [latlons (keys (mk-sorted-map (mk-empty-latlon-map lats lons)))
        latlon-split (map #(.split % ",") latlons)]
    (latlons->wkt-multi-point (map first latlon-split)
                              (map second latlon-split))))

(defn extract
  "Extract a specific field by index from tuples."
  [field tuples]
  (map #(nth % field) tuples))

(defn extract-field
  "Extracts a particular field (indexed by field-num), creates an
   ordered map with \"lat,lon\" keys, and returns the values in order."
  [tuples lats lons field-num]
  (->> (extract field-num tuples)
       (collect-latlon-vals->map lats lons)
       (mk-sorted-map)
       (vals)))

(defn valid-name?
  "Return true if name is valid, otherwise return false."
  [name]
  (and (not= name nil) (not= name "")))

(defn valid-latlon?
  "Return true if lat and lon are valid decimal degrees,
   otherwise return false. Assumes that lat and lon are both either numeric
   or string."
  [lat lon]
  (let [[lat lon] (if (number? lat)
                    [lat lon]
                    (map read-string [lat lon]))
        latlon-range {:lat-min -90 :lat-max 90 :lon-min -180 :lon-max 180}
        {:keys [lat-min lat-max lon-min lon-max]} latlon-range]
    (and (<= lat lat-max)
         (>= lat lat-min)
         (<= lon lon-max)
         (>= lon lon-min))))

(defn str->num-or-empty-str
  "Convert a string to a number with read-string and return it. If not a
   number, return an empty string.

   Try/catch form will catch exception from using read-string with
   non-decimal degree or entirely wrong lats and lons (a la 5°52.5'N, 6d
   10m s S, or 0/0/0 - all have been seen in the data).

   Note that this will also handle major errors in the lat/lon fields
   that may be due to mal-formed or non-standard input text lines that
   would otherwise cause parsing errors."
  [s]
  (try
    (let [parsed-str (read-string s)]
      (if (number? parsed-str)
        parsed-str
        ""))
    (catch Exception e "")))

(defn handle-zeros
  "Handle trailing decimal points and trailing zeros. A trailing decimal
   point is removed entirely, while trailing zeros are only dropped if
   they immediately follow the decimal point.

   Usage:
     (handle-zeros \"3.\")
     ;=> \"3\"

     (handle-zeros \"3.0\")
     ;=> \"3\"

     (handle-zeros \"3.00\")
     ;=> \"3\"

     (handle-zeros \"3.001\")
     ;=> \"3.001\"

     (handle-zeros \"3.00100\")
     ;=>\"3.00100\""
  [s]
  (let [[head tail] (clojure.string/split s #"\.")]
    (if (or (zero? (count tail)) ;; nothing after decimal place
            (zero? (Integer/parseInt tail))) ;; all zeros after decimal place
      (str (Integer/parseInt head))
      s)))

(defn round-to
  "Round a value to a given number of decimal places and return a
   string. Note that this will drop all trailing zeros, and values like
   3.0 will be returned as \"3\""
  [digits n]
  (let [formatter (str "%." (str digits) "f")]
    (if (= "" n)
      n
      (->> (format formatter (double n))
           reverse
           (drop-while #{\0})
           reverse
           (apply str)
           (handle-zeros)))))

(defn parse-hemisphere
  "Returns a quarter->season map based on the hemisphere."
  [h]
  (let [n_seasons {0 "winter" 1 "spring" 2 "summer" 3 "fall"}
        s_seasons {0 "summer" 1 "fall" 2 "winter" 3 "spring"}]
    (if (= h "N") n_seasons s_seasons)))

(defn get-season-idx
  "Returns season index (roughly quarter) given a month."
  [month]
  {:pre [(>= 12 month)]}
  (let [season-idxs {11 0 12 0 1 0
                     2 1 3 1 4 1
                     5 2 6 2 7 2
                     8 3 9 3 10 3}]
    (get season-idxs month)))

(defn get-season
  "Based on the latitude and the month, return a \"hemisphere season\"
   string.

   Usage:
     (get-season 40.0 1)
     ;=> \"N winter\""
  [lat month]
  (let [lat (if (string? lat) (read-string lat) lat)
        month (if (string? month) (read-string month) month)
        hemisphere (if (pos? lat) "N" "S")
        season (get (parse-hemisphere hemisphere)
                    (get-season-idx month))]
    (format "%s %s" hemisphere season)))

(defn surround-str
  "Surround a supplied string with supplied string.

   Usage:
     (surround-str \"yomama\" \"'\")
     ;=> \"'yomamma'\""
  [s surround-with]
  (format "%s%s%s" surround-with s surround-with))

(defn concat-results
  "Concatenate a collection of strings, with an optional separator."
  [results-vec & [sep]]
  (apply str (interpose sep results-vec)))

(defn prep-vals
  "Format collection for insert, including adding quotes and {}.

   Usage:
     (u/prep-vals [[\"1223445\" \"2302043\"] [\"2132424\"]])
     ;=> \"'{\"1223445,2302043\", \"2132424\"}'\"

     Looks nicer if you print it with `println`"
  [coll]
  (->> coll
       (map #(concat-results % ","))
       (map #(surround-str % "\""))
       (#(concat-results % ", "))
       (format "{%s}")
       (#(surround-str % "'"))))

(defn mk-value-str
  "Given various fields, make comma-separated value string for insert
   statement."
  [name occ-ids precisions years months seasons]
  (-> [(surround-str name "'") (prep-vals occ-ids)
       (prep-vals precisions) (prep-vals years)
       (prep-vals months) (prep-vals seasons)]
      (concat-results ", ")))

(defn mk-insert-stmt
  "Create final insert statement given a value string and multi-point
  string"
  [value-str multi-point]
  (let [s (str "INSERT INTO gbif_points (name, occids, precision, year, month,"
               " season, the_geom_multipoint) values (%s, ST_GeomFromText('%s', 4326))")]
    (format s value-str multi-point)))
