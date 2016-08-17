(ns adoptions.core
  (:gen-class))

(defn credit-check [person]
  ; assume some work is done to compute the credit check
  (Thread/sleep 1000)
  750)

(defn home-check [person]
  (Thread/sleep 10000)
  true)

(defn pet-friendly [person adopting]
  (Thread/sleep 4000)
  (let [numpets (count (:pets person))]
    (and
      (= numpets
         ; filter the list of pets to contain those that are ok with adopting
         (count (filter #((:friendly-with %) (:species adopting))
                        (:pets person))))
      (= numpets
         ; filter the list of pets to contain those that the adopting is ok with
         (count (filter #((:friendly-with adopting) (:species %))
                        (:pets person)))))))

(defn can-adopt? [person animal]
  (let [credit (future (credit-check person))
        home (future (home-check person))
        pets (future (pet-friendly person animal))]
    (cond
      (< @credit 500) false
      (not @home) false
      (not @pets) false
      :else true)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (can-adopt? {:name "Neal" :pets [{:species "iguana" :name "Indy" :friendly-with #{"dog" "rat" "cat"}}
                                       {:species "rat" :name "Peyton" :friendly-with #{"dog" "rat"}}]}
                  {:species "dog" :friendly-with #{"iguana" "rat"}})
    (println "You can adopt!")
    (println "Sorry :(")))
