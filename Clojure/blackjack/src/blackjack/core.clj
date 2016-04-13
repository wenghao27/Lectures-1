(ns blackjack.core
  (:gen-class))

(declare greedy-player-strategy many-games cautious-player-strategy inactive-player-strategy)

(defn new-deck []
  (for [suit (range 4)
        kind (range 1 14)]
    {:suit suit :kind kind}))

(defn card-string [card]
  (let [suits ["Spades" "Clubs" "Diamonds" "Hearts"]]
    (str (case (:kind card)
           1 "Ace"
           11 "Jack"
           12 "Queen"
           13 "King"
           (str (:kind card)))
         " of " (nth suits (:suit card)))))

(defn new-game []
  (let [deck (shuffle (new-deck))]
    {:player-hand (list (first deck) (second deck))
     :dealer-hand (list (nth deck 2) (nth deck 3))
     :deck (-> deck next next next next)}))

(defn card-value [card]
  (cond (= 1 (:kind card)) 11
        (<= 10 (:kind card)) 10
        :else (:kind card)))

(defn hand-total [hand]
  (let [sum (reduce + 0 (map card-value hand))
        aces (count (filter #(= 1 (:kind %)) hand))]
    (if (or (<= sum 22) (= 0 aces))
      sum
      (- sum (* 10 (min aces (int (Math/ceil (/ (- sum 21) 10)))))))))

(defn hit [game-state which-player]
  (let [nextcard (first (:deck game-state))
        deck (which-player game-state)
        newdeck (cons nextcard deck)]
    (assoc game-state
      which-player newdeck
      :deck (next (:deck game-state)))))

(defn user-strategy [game-state]
  "Lets the user decide whether to hit"
  (if (< (hand-total (:player-hand game-state)) 21)
    (do (println "hit or stay?")
        (let [input (read-line)]
          (if (= "hit" input)
            (hit game-state :player-hand)
            game-state)))
    game-state))

(defn dealer-strategy [game-state]
  "Dealer always hits if less than 17"
  (if (< (hand-total (:dealer-hand game-state)) 17)
    (hit game-state :dealer-hand)
    game-state))

(defn do-turn [game-state strategy]
  (strategy game-state))

(defn user-turn [game-state]
  (let [hand (:player-hand game-state)
        score (hand-total hand)]
    (println "Player hand:" (clojure.string/join ", " (map card-string hand))
             ";" score "points")
    (if (< score 21)
      (let [next-state (do-turn game-state inactive-player-strategy)]
        (if (not= (count hand) (count (:player-hand next-state)))
          (user-turn next-state)
          next-state))
      game-state)))

(defn dealer-turn [game-state]
  (let [hand (:dealer-hand game-state)
        score (hand-total hand)]
  (println "Dealer's hand:" (clojure.string/join ", " (map card-string hand))
           ";" score "points")
  (let [next-state (do-turn game-state dealer-strategy)]
    (if (and (<= (hand-total (:player-hand game-state)) 21)
             (not= (count hand) (count (:dealer-hand next-state))))
      (do (println "Dealer hits")
          (dealer-turn next-state))
      next-state))))

(defn one-game [game-state]
  (println "Dealer is showing" (card-string (first (:dealer-hand game-state))))
  (let [end-game (dealer-turn (user-turn game-state))
        dealer-score (hand-total (:dealer-hand end-game))
        player-score (hand-total (:player-hand end-game))]
    (if (and (<= player-score 21) (or (> player-score dealer-score) (> dealer-score 21)))
      1
      (if (= player-score dealer-score)
        0
        -1))))

(defn -main
  [& args]
  ;(let [winner (one-game (new-game))]
  ;  (println (case winner
  ;             1 "Player wins!"
  ;             -1 "Dealer wins!"
  ;             :else "Tie game")))
  ;)
  (let [totals (many-games 1000 0 0 0)]
    (println totals)))

(defn many-games [n player-wins dealer-wins ties]
  (let [winner (one-game (new-game))]
    (if (= n 0)
      {:player-wins player-wins :dealer-wins dealer-wins :ties ties}
      (many-games
        (- n 1)
        (if (= winner 1) (+ player-wins 1) player-wins)
        (if (= winner -1) (+ dealer-wins 1) dealer-wins)
        (if (= winner 0) (+ ties 1) ties)))))

(defn greedy-player-strategy [game-state]
  "A greedy player always hits until they get 21 or bust"
  (if (< (hand-total (:player-hand game-state)) 21)
    (do (println "Player hits")
        (hit game-state :player-hand))
    (do (println "Player stays")
        game-state)))

(defn cautious-player-strategy [game-state]
  "A cautious player will only hit when under 15"
  (if (< (hand-total (:player-hand game-state)) 15)
    (do (println "Player hits")
        (hit game-state :player-hand))
    (do (println "P-layer stays")
        game-state)))

(defn inactive-player-strategy [game-state]
  "An inactive player never hits"
  (do (println "Player stays")
      game-state))