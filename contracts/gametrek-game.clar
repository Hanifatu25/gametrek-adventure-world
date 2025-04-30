;; gametrek-game
;; This contract serves as the core engine for the GameTrek adventure world, managing
;; all aspects of the game including characters, items, quests, battles, and player interactions.
;; All game assets and progress are stored on-chain, ensuring true ownership and persistence
;; of the game world state.

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-CHARACTER-NOT-FOUND (err u1002))
(define-constant ERR-ITEM-NOT-FOUND (err u1003))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1004))
(define-constant ERR-INVALID-PARAMS (err u1005))
(define-constant ERR-CHARACTER-EXISTS (err u1006))
(define-constant ERR-INVENTORY-FULL (err u1007))
(define-constant ERR-LEVEL-REQUIREMENT (err u1008))
(define-constant ERR-QUEST-NOT-AVAILABLE (err u1009))
(define-constant ERR-QUEST-ALREADY-COMPLETED (err u1010))
(define-constant ERR-INSUFFICIENT-HEALTH (err u1011))
(define-constant ERR-ITEM-NOT-TRANSFERABLE (err u1012))
(define-constant ERR-MONSTER-NOT-FOUND (err u1013))
(define-constant ERR-ITEM-EQUIPPED (err u1014))

;; Game configuration constants
(define-constant MAX-INVENTORY-SIZE u20)
(define-constant MAX-LEVEL u100)
(define-constant BASE-HEALTH u100)
(define-constant BASE-ATTACK u10)
(define-constant BASE-DEFENSE u5)
(define-constant XP-PER-LEVEL u1000)
(define-constant CONTRACT-OWNER tx-sender)

;; Data maps and variables

;; Character data - stores all attributes for a player character
(define-map characters 
  { id: uint }
  {
    owner: principal,
    name: (string-ascii 30),
    level: uint,
    xp: uint,
    health: uint,
    max-health: uint,
    attack: uint,
    defense: uint,
    created-at: uint,
    location: (string-ascii 30)
  }
)

;; Item data - stores information about game items
(define-map items
  { id: uint }
  {
    owner: principal,
    name: (string-ascii 30),
    item-type: (string-ascii 20), ;; weapon, armor, potion, collectible, etc.
    rarity: (string-ascii 20), ;; common, uncommon, rare, epic, legendary
    attack-bonus: uint,
    defense-bonus: uint,
    health-bonus: uint,
    level-requirement: uint,
    equipped: bool,
    transferable: bool,
    created-at: uint
  }
)

;; Inventory tracking - maps characters to their items
(define-map character-inventory
  { character-id: uint }
  { item-ids: (list 50 uint) }
)

;; Equipped items - tracks which items a character has equipped
(define-map equipped-items
  { character-id: uint }
  {
    weapon: (optional uint),
    armor: (optional uint),
    accessory: (optional uint)
  }
)

;; Quest data - stores information about available quests
(define-map quests
  { id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    level-requirement: uint,
    xp-reward: uint,
    item-rewards: (list 10 uint),
    location: (string-ascii 30)
  }
)

;; Monster data - information about monsters that can be battled
(define-map monsters
  { id: uint }
  {
    name: (string-ascii 30),
    health: uint,
    attack: uint,
    defense: uint,
    level: uint,
    xp-reward: uint,
    location: (string-ascii 30),
    possible-drops: (list 10 uint),
    drop-rates: (list 10 uint)
  }
)

;; Completed quests - tracks which quests each character has completed
(define-map completed-quests
  { character-id: uint }
  { quest-ids: (list 100 uint) }
)

;; Global counters for ID generation
(define-data-var next-character-id uint u1)
(define-data-var next-item-id uint u1)
(define-data-var next-quest-id uint u1)
(define-data-var next-monster-id uint u1)

;; Private functions

;; Generate a new character ID
(define-private (generate-character-id)
  (let ((id (var-get next-character-id)))
    (var-set next-character-id (+ id u1))
    id))

;; Generate a new item ID
(define-private (generate-item-id)
  (let ((id (var-get next-item-id)))
    (var-set next-item-id (+ id u1))
    id))

;; Check if caller is character owner
(define-private (is-character-owner (character-id uint))
  (let ((character-info (unwrap! (map-get? characters { id: character-id }) false)))
    (is-eq (get owner character-info) tx-sender)))

;; Check if caller is item owner
(define-private (is-item-owner (item-id uint))
  (let ((item-info (unwrap! (map-get? items { id: item-id }) false)))
    (is-eq (get owner item-info) tx-sender)))

;; Calculate health for a given level
(define-private (calculate-health (level uint))
  (+ BASE-HEALTH (* level u10)))

;; Calculate attack for a given level
(define-private (calculate-attack (level uint))
  (+ BASE-ATTACK (* level u2)))

;; Calculate defense for a given level
(define-private (calculate-defense (level uint))
  (+ BASE-DEFENSE level))

;; Check if character can level up and process level up if possible
(define-private (check-and-process-level-up (character-id uint))
  (let ((character-info (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (current-level (get level character-info))
        (current-xp (get xp character-info))
        (xp-needed (* current-level XP-PER-LEVEL)))
    (if (>= current-xp xp-needed)
        (if (< current-level MAX-LEVEL)
            (let ((new-level (+ current-level u1))
                  (remaining-xp (- current-xp xp-needed))
                  (new-max-health (calculate-health new-level))
                  (new-attack (calculate-attack new-level))
                  (new-defense (calculate-defense new-level)))
              (map-set characters
                { id: character-id }
                (merge character-info 
                  { 
                    level: new-level,
                    xp: remaining-xp,
                    max-health: new-max-health,
                    health: new-max-health,
                    attack: new-attack,
                    defense: new-defense
                  }
                )
              )
              (ok true))
            (ok false)) ;; Already at max level
        (ok false)))) ;; Not enough XP

;; Add an item to character's inventory
(define-private (add-to-inventory (character-id uint) (item-id uint))
  (let ((inventory (default-to { item-ids: (list) } (map-get? character-inventory { character-id: character-id })))
        (items-list (get item-ids inventory)))
    (if (< (len items-list) MAX-INVENTORY-SIZE)
        (begin
          (map-set character-inventory
            { character-id: character-id }
            { item-ids: (append items-list item-id) }
          )
          (ok true))
        ERR-INVENTORY-FULL)))

;; Remove an item from character's inventory
(define-private (remove-from-inventory (character-id uint) (item-id uint))
  (let ((inventory (unwrap! (map-get? character-inventory { character-id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (items-list (get item-ids inventory))
        (filtered-items (filter (lambda (id) (not (is-eq id item-id))) items-list)))
    (map-set character-inventory
      { character-id: character-id }
      { item-ids: filtered-items }
    )
    (ok true)))

;; Check if quest is completed by character
(define-private (is-quest-completed (character-id uint) (quest-id uint))
  (let ((completed (default-to { quest-ids: (list) } (map-get? completed-quests { character-id: character-id }))))
    (is-some (index-of (get quest-ids completed) quest-id))))

;; Generate pseudorandom number - Note: This is not cryptographically secure
;; In a production environment, consider using a VRF or similar solution
(define-private (pseudo-random (seed uint) (max uint))
  (let ((hash (unwrap-panic (contract-call? 'SP000000000000000000002Q6VF78.bns token-uri seed)))
        (hash-uint (if (> (len hash) u1) 
                      (string-to-uint (slice hash u0 u10))
                      u123456789)))
    (mod hash-uint max)))

;; String to uint conversion helper
(define-private (string-to-uint (str (string-ascii 50)))
  (default-to u0 (string-to-uint-helper str u0 u0)))

(define-private (string-to-uint-helper (str (string-ascii 50)) (index uint) (acc uint))
  (if (>= index (len str))
      (some acc)
      (let ((char (unwrap-panic (element-at str index))))
        (if (and (>= (to-uint char) (to-uint u"0")) (<= (to-uint char) (to-uint u"9")))
            (string-to-uint-helper 
              str 
              (+ index u1) 
              (+ (* acc u10) (- (to-uint char) (to-uint u"0"))))
            none))))

;; Calculate battle outcome between character and monster
(define-private (calculate-battle-outcome (character-id uint) (monster-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (monster (unwrap! (map-get? monsters { id: monster-id }) ERR-MONSTER-NOT-FOUND))
        (character-attack (get attack character))
        (character-defense (get defense character))
        (character-health (get health character))
        (monster-attack (get attack monster))
        (monster-defense (get defense monster))
        (monster-health (get health monster)))
    
    ;; Simple battle simulation
    (let ((damage-to-monster (max u1 (- character-attack monster-defense)))
          (damage-to-character (max u1 (- monster-attack character-defense)))
          (rounds-to-kill-monster (/ (+ monster-health damage-to-monster u1) damage-to-monster))
          (rounds-to-kill-character (/ (+ character-health damage-to-character u1) damage-to-character)))
      
      (if (<= rounds-to-kill-monster rounds-to-kill-character)
          ;; Character wins
          (let ((health-remaining (- character-health (* damage-to-character rounds-to-kill-monster))))
            {
              victor: "character",
              remaining-health: health-remaining,
              xp-gained: (get xp-reward monster)
            })
          ;; Monster wins
          {
            victor: "monster",
            remaining-health: u0,
            xp-gained: u0
          }))))

;; Read-only functions

;; Get character information
(define-read-only (get-character (character-id uint))
  (map-get? characters { id: character-id }))

;; Get item information
(define-read-only (get-item (item-id uint))
  (map-get? items { id: item-id }))

;; Get character inventory
(define-read-only (get-inventory (character-id uint))
  (map-get? character-inventory { character-id: character-id }))

;; Get quest information
(define-read-only (get-quest (quest-id uint))
  (map-get? quests { id: quest-id }))

;; Get monster information
(define-read-only (get-monster (monster-id uint))
  (map-get? monsters { id: monster-id }))

;; Check if a character meets level requirements for an item
(define-read-only (meets-level-requirement (character-id uint) (item-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) (ok false)))
        (item (unwrap! (map-get? items { id: item-id }) (ok false))))
    (ok (>= (get level character) (get level-requirement item)))))

;; Get a list of all quests available to a character based on level
(define-read-only (get-available-quests (character-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (character-level (get level character))
        (character-location (get location character)))
    (ok { level: character-level, location: character-location })))

;; Public functions

;; Create a new character
(define-public (create-character (name (string-ascii 30)))
  (let ((new-id (generate-character-id))
        (current-time (default-to u0 block-height)))
    (map-set characters
      { id: new-id }
      {
        owner: tx-sender,
        name: name,
        level: u1,
        xp: u0,
        health: BASE-HEALTH,
        max-health: BASE-HEALTH,
        attack: BASE-ATTACK,
        defense: BASE-DEFENSE,
        created-at: current-time,
        location: "starter-village"
      }
    )
    ;; Initialize empty inventory
    (map-set character-inventory
      { character-id: new-id }
      { item-ids: (list) }
    )
    ;; Initialize empty equipped items
    (map-set equipped-items
      { character-id: new-id }
      {
        weapon: none,
        armor: none,
        accessory: none
      }
    )
    ;; Initialize empty completed quests
    (map-set completed-quests
      { character-id: new-id }
      { quest-ids: (list) }
    )
    (ok new-id)))

;; Create a new game item (admin only)
(define-public (create-item 
  (name (string-ascii 30))
  (item-type (string-ascii 20))
  (rarity (string-ascii 20))
  (attack-bonus uint)
  (defense-bonus uint)
  (health-bonus uint)
  (level-requirement uint)
  (transferable bool))
  
  (if (is-eq tx-sender CONTRACT-OWNER)
      (let ((new-id (generate-item-id))
            (current-time (default-to u0 block-height)))
        (map-set items
          { id: new-id }
          {
            owner: CONTRACT-OWNER,
            name: name,
            item-type: item-type,
            rarity: rarity,
            attack-bonus: attack-bonus,
            defense-bonus: defense-bonus,
            health-bonus: health-bonus,
            level-requirement: level-requirement,
            equipped: false,
            transferable: transferable,
            created-at: current-time
          }
        )
        (ok new-id))
      ERR-NOT-AUTHORIZED))

;; Create a new quest (admin only)
(define-public (create-quest
  (name (string-ascii 50))
  (description (string-ascii 200))
  (level-requirement uint)
  (xp-reward uint)
  (item-rewards (list 10 uint))
  (location (string-ascii 30)))
  
  (if (is-eq tx-sender CONTRACT-OWNER)
      (let ((new-id (var-get next-quest-id)))
        (var-set next-quest-id (+ new-id u1))
        (map-set quests
          { id: new-id }
          {
            name: name,
            description: description,
            level-requirement: level-requirement,
            xp-reward: xp-reward,
            item-rewards: item-rewards,
            location: location
          }
        )
        (ok new-id))
      ERR-NOT-AUTHORIZED))

;; Create a new monster (admin only)
(define-public (create-monster
  (name (string-ascii 30))
  (health uint)
  (attack uint)
  (defense uint)
  (level uint)
  (xp-reward uint)
  (location (string-ascii 30))
  (possible-drops (list 10 uint))
  (drop-rates (list 10 uint)))
  
  (if (is-eq tx-sender CONTRACT-OWNER)
      (let ((new-id (var-get next-monster-id)))
        (var-set next-monster-id (+ new-id u1))
        (map-set monsters
          { id: new-id }
          {
            name: name,
            health: health,
            attack: attack,
            defense: defense,
            level: level,
            xp-reward: xp-reward,
            location: location,
            possible-drops: possible-drops,
            drop-rates: drop-rates
          }
        )
        (ok new-id))
      ERR-NOT-AUTHORIZED))

;; Transfer character ownership
(define-public (transfer-character (character-id uint) (recipient principal))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND)))
    (if (is-eq (get owner character) tx-sender)
        (begin
          (map-set characters
            { id: character-id }
            (merge character { owner: recipient })
          )
          (ok true))
        ERR-NOT-AUTHORIZED)))

;; Transfer item ownership
(define-public (transfer-item (item-id uint) (recipient principal))
  (let ((item (unwrap! (map-get? items { id: item-id }) ERR-ITEM-NOT-FOUND)))
    (if (and
          (is-eq (get owner item) tx-sender)
          (get transferable item)
          (not (get equipped item)))
        (begin
          (map-set items
            { id: item-id }
            (merge item { owner: recipient })
          )
          (ok true))
        (if (not (get transferable item))
            ERR-ITEM-NOT-TRANSFERABLE
            (if (get equipped item)
                ERR-ITEM-EQUIPPED
                ERR-NOT-AUTHORIZED)))))

;; Equip an item
(define-public (equip-item (character-id uint) (item-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (item (unwrap! (map-get? items { id: item-id }) ERR-ITEM-NOT-FOUND))
        (equipped-map (unwrap! (map-get? equipped-items { character-id: character-id }) ERR-CHARACTER-NOT-FOUND)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    (asserts! (is-item-owner item-id) ERR-NOT-AUTHORIZED)
    
    ;; Check level requirements
    (asserts! (>= (get level character) (get level-requirement item)) ERR-LEVEL-REQUIREMENT)
    
    ;; Perform equipment based on item type
    (let ((item-type (get item-type item))
          (updated-equipped-map equipped-map)
          (attack-bonus (get attack-bonus item))
          (defense-bonus (get defense-bonus item))
          (health-bonus (get health-bonus item)))
      
      ;; Update the appropriate equipment slot
      (if (is-eq item-type "weapon")
          (set! updated-equipped-map (merge equipped-map { weapon: (some item-id) }))
          (if (is-eq item-type "armor")
              (set! updated-equipped-map (merge equipped-map { armor: (some item-id) }))
              (if (is-eq item-type "accessory")
                  (set! updated-equipped-map (merge equipped-map { accessory: (some item-id) }))
                  ERR-INVALID-PARAMS)))
      
      ;; Update character stats and mark item as equipped
      (map-set characters
        { id: character-id }
        (merge character 
          { 
            attack: (+ (get attack character) attack-bonus),
            defense: (+ (get defense character) defense-bonus),
            max-health: (+ (get max-health character) health-bonus),
            health: (+ (get health character) health-bonus)
          }
        )
      )
      
      (map-set items
        { id: item-id }
        (merge item { equipped: true })
      )
      
      (map-set equipped-items
        { character-id: character-id }
        updated-equipped-map
      )
      
      (ok true))))

;; Unequip an item
(define-public (unequip-item (character-id uint) (item-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (item (unwrap! (map-get? items { id: item-id }) ERR-ITEM-NOT-FOUND))
        (equipped-map (unwrap! (map-get? equipped-items { character-id: character-id }) ERR-CHARACTER-NOT-FOUND)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    (asserts! (is-item-owner item-id) ERR-NOT-AUTHORIZED)
    
    ;; Check if item is actually equipped
    (asserts! (get equipped item) ERR-INVALID-PARAMS)
    
    ;; Perform unequip based on item type
    (let ((item-type (get item-type item))
          (updated-equipped-map equipped-map)
          (attack-bonus (get attack-bonus item))
          (defense-bonus (get defense-bonus item))
          (health-bonus (get health-bonus item)))
      
      ;; Update the appropriate equipment slot
      (if (is-eq item-type "weapon")
          (set! updated-equipped-map (merge equipped-map { weapon: none }))
          (if (is-eq item-type "armor")
              (set! updated-equipped-map (merge equipped-map { armor: none }))
              (if (is-eq item-type "accessory")
                  (set! updated-equipped-map (merge equipped-map { accessory: none }))
                  ERR-INVALID-PARAMS)))
      
      ;; Update character stats and mark item as unequipped
      (map-set characters
        { id: character-id }
        (merge character 
          { 
            attack: (- (get attack character) attack-bonus),
            defense: (- (get defense character) defense-bonus),
            max-health: (- (get max-health character) health-bonus),
            ;; Don't reduce current health below 1
            health: (max u1 (- (get health character) health-bonus))
          }
        )
      )
      
      (map-set items
        { id: item-id }
        (merge item { equipped: false })
      )
      
      (map-set equipped-items
        { character-id: character-id }
        updated-equipped-map
      )
      
      (ok true))))

;; Start a quest
(define-public (start-quest (character-id uint) (quest-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (quest (unwrap! (map-get? quests { id: quest-id }) ERR-QUEST-NOT-AVAILABLE)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    
    ;; Check level requirements
    (asserts! (>= (get level character) (get level-requirement quest)) ERR-LEVEL-REQUIREMENT)
    
    ;; Check quest hasn't been completed
    (asserts! (not (is-quest-completed character-id quest-id)) ERR-QUEST-ALREADY-COMPLETED)
    
    ;; Check location match
    (asserts! (is-eq (get location character) (get location quest)) ERR-QUEST-NOT-AVAILABLE)
    
    ;; Award XP
    (let ((new-xp (+ (get xp character) (get xp-reward quest))))
      (map-set characters
        { id: character-id }
        (merge character { xp: new-xp })
      )
      
      ;; Award items
      (let ((item-rewards (get item-rewards quest)))
        (map add-to-inventory (list character-id) item-rewards)
      )
      
      ;; Mark quest as completed
      (let ((completed (default-to { quest-ids: (list) } (map-get? completed-quests { character-id: character-id }))))
        (map-set completed-quests
          { character-id: character-id }
          { quest-ids: (append (get quest-ids completed) quest-id) }
        )
      )
      
      ;; Check if character leveled up
      (check-and-process-level-up character-id)
      
      (ok true))))

;; Battle a monster
(define-public (battle-monster (character-id uint) (monster-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (monster (unwrap! (map-get? monsters { id: monster-id }) ERR-MONSTER-NOT-FOUND)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    
    ;; Check character has health
    (asserts! (> (get health character) u0) ERR-INSUFFICIENT-HEALTH)
    
    ;; Check location match
    (asserts! (is-eq (get location character) (get location monster)) ERR-INVALID-PARAMS)
    
    ;; Calculate battle outcome
    (let ((battle-result (calculate-battle-outcome character-id monster-id)))
      (if (is-eq (get victor battle-result) "character")
          (begin
            ;; Character won - update health, award XP
            (let ((new-health (get remaining-health battle-result))
                  (xp-gained (get xp-gained battle-result))
                  (new-xp (+ (get xp character) xp-gained)))
              
              (map-set characters
                { id: character-id }
                (merge character 
                  { 
                    health: new-health,
                    xp: new-xp
                  }
                )
              )
              
              ;; Determine if character gets item drops
              (let ((possible-drops (get possible-drops monster))
                    (drop-rates (get drop-rates monster))
                    (seed (default-to u12345 block-height)))
                
                ;; Simplified drop system
                (let ((drop-roll (pseudo-random seed u100)))
                  (if (< drop-roll u50) ;; 50% chance to get an item
                      (let ((drop-index (mod drop-roll (len possible-drops))))
                        (let ((dropped-item-id (unwrap! (element-at possible-drops drop-index) 
                                                      (ok { success: true, dropped-item: none }))))
                          ;; Add item to inventory
                          (add-to-inventory character-id dropped-item-id)
                          ;; Check for level up
                          (check-and-process-level-up character-id)
                          (ok { success: true, dropped-item: (some dropped-item-id) })
                        ))
                      (begin
                        ;; Check for level up even if no item dropped
                        (check-and-process-level-up character-id)
                        (ok { success: true, dropped-item: none })
                      ))
                ))
            ))
          ;; Character lost
          (begin
            ;; Set health to 1
            (map-set characters
              { id: character-id }
              (merge character { health: u1 })
            )
            (ok { success: false, dropped-item: none })
          )))))

;; Travel to a new location
(define-public (travel (character-id uint) (new-location (string-ascii 30)))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    
    ;; Update location
    (map-set characters
      { id: character-id }
      (merge character { location: new-location })
    )
    
    (ok true)))

;; Heal character
(define-public (heal-character (character-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    
    ;; Restore health to max
    (map-set characters
      { id: character-id }
      (merge character { health: (get max-health character) })
    )
    
    (ok true)))

;; Use a consumable item (like potion)
(define-public (use-consumable (character-id uint) (item-id uint))
  (let ((character (unwrap! (map-get? characters { id: character-id }) ERR-CHARACTER-NOT-FOUND))
        (item (unwrap! (map-get? items { id: item-id }) ERR-ITEM-NOT-FOUND)))
    
    ;; Verify ownership
    (asserts! (is-character-owner character-id) ERR-NOT-AUTHORIZED)
    (asserts! (is-item-owner item-id) ERR-NOT-AUTHORIZED)
    
    ;; Check if item is a consumable
    (asserts! (is-eq (get item-type item) "potion") ERR-INVALID-PARAMS)
    
    ;; Apply item effects (health bonus for potions)
    (let ((health-bonus (get health-bonus item))
          (current-health (get health character))
          (max-health (get max-health character))
          (new-health (min max-