;; =====================================================
;; CLARINET PROJECT: Decentralized Community Water Stewardship
;; =====================================================


;; =====================================================
;; CONTRACT 1: water-quality-monitor.clar
;; Core water quality monitoring, pollution tracking, and ecosystem health
;; =====================================================

;; Contract: water-quality-monitor
;; Purpose: Manages water quality data, pollution sources, and ecosystem health metrics

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-PARAMS (err u400))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INSUFFICIENT-STAKE (err u402))
(define-constant ERR-INSUFFICIENT-FUNDS (err u403))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-VALIDATOR-STAKE u1000000) ;; 1 STX minimum stake
(define-constant DATA-VALIDITY-PERIOD u144) ;; ~24 hours in blocks

;; Data structures
(define-map watersheds
  { watershed-id: uint }
  {
    name: (string-ascii 100),
    coordinates: (tuple (lat int) (lng int)),
    area-hectares: uint,
    created-at: uint,
    steward: principal,
    ecosystem-type: (string-ascii 50),
    indigenous-name: (optional (string-ascii 100))
  }
)

(define-map quality-readings
  { reading-id: uint }
  {
    watershed-id: uint,
    timestamp: uint,
    validator: principal,
    ph-level: uint, ;; multiplied by 100 (7.5 = 750)
    dissolved-oxygen: uint, ;; ppm * 100
    turbidity: uint, ;; NTU * 100
    temperature: uint, ;; celsius * 100
    nitrate-level: uint, ;; mg/L * 100
    phosphate-level: uint, ;; mg/L * 100
    bacterial-count: uint, ;; CFU/100mL
    heavy-metals: uint, ;; composite score * 100
    verified: bool,
    verification-count: uint
  }
)

(define-map pollution-sources
  { source-id: uint }
  {
    watershed-id: uint,
    type: (string-ascii 50), ;; industrial, agricultural, urban, natural
    severity: uint, ;; 1-10 scale
    coordinates: (tuple (lat int) (lng int)),
    identified-by: principal,
    timestamp: uint,
    status: (string-ascii 20), ;; active, mitigated, resolved
    description: (string-ascii 500)
  }
)

(define-map ecosystem-health
  { watershed-id: uint, assessment-date: uint }
  {
    biodiversity-index: uint, ;; 0-1000 scale
    habitat-quality: uint, ;; 0-100 scale
    species-count: uint,
    invasive-species-present: bool,
    vegetation-coverage: uint, ;; percentage * 100
    erosion-level: uint, ;; 1-10 scale
    assessed-by: principal,
    indigenous-indicators: (optional (string-ascii 200))
  }
)

(define-map usage-tracking
  { usage-id: uint }
  {
    watershed-id: uint,
    usage-type: (string-ascii 30), ;; municipal, agricultural, industrial, recreational
    volume-liters: uint,
    date: uint,
    reported-by: principal,
    sustainable-rating: uint ;; 1-10 scale
  }
)

(define-map authorized-validators
  { validator: principal }
  {
    stake-amount: uint,
    reputation-score: uint,
    certifications: (list 5 (string-ascii 50)),
    active: bool,
    joined-at: uint
  }
)

;; Counters
(define-data-var watershed-counter uint u0)
(define-data-var reading-counter uint u0)
(define-data-var source-counter uint u0)
(define-data-var usage-counter uint u0)

;; Administrative functions
(define-public (register-watershed
    (name (string-ascii 100))
    (lat int)
    (lng int)
    (area uint)
    (ecosystem-type (string-ascii 50))
    (indigenous-name (optional (string-ascii 100))))
  (let ((watershed-id (+ (var-get watershed-counter) u1)))
    (asserts! (> (len name) u0) ERR-INVALID-PARAMS)
    (asserts! (> area u0) ERR-INVALID-PARAMS)

    (var-set watershed-counter watershed-id)
    (ok (map-set watersheds
      { watershed-id: watershed-id }
      {
        name: name,
        coordinates: { lat: lat, lng: lng },
        area-hectares: area,
        created-at: stacks-block-height,
        steward: tx-sender,
        ecosystem-type: ecosystem-type,
        indigenous-name: indigenous-name
      }))))

(define-public (become-validator (certifications (list 5 (string-ascii 50))))
  (begin
    (asserts! (>= (stx-get-balance tx-sender) MIN-VALIDATOR-STAKE) ERR-INSUFFICIENT-STAKE)
    (asserts! (is-none (map-get? authorized-validators { validator: tx-sender })) ERR-ALREADY-EXISTS)

    (try! (stx-transfer? MIN-VALIDATOR-STAKE tx-sender (as-contract tx-sender)))

    (ok (map-set authorized-validators
      { validator: tx-sender }
      {
        stake-amount: MIN-VALIDATOR-STAKE,
        reputation-score: u100,
        certifications: certifications,
        active: true,
        joined-at: stacks-block-height
      }))))

;; Core monitoring functions
(define-public (submit-quality-reading
    (watershed-id uint)
    (ph uint)
    (oxygen uint)
    (turbidity uint)
    (temperature uint)
    (nitrates uint)
    (phosphates uint)
    (bacteria uint)
    (metals uint))
  (let ((reading-id (+ (var-get reading-counter) u1))
        (validator-info (unwrap! (map-get? authorized-validators { validator: tx-sender }) ERR-UNAUTHORIZED)))

    (asserts! (get active validator-info) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? watersheds { watershed-id: watershed-id })) ERR-NOT-FOUND)
    (asserts! (and (>= ph u100) (<= ph u1400)) ERR-INVALID-PARAMS) ;; pH 1.0-14.0

    (var-set reading-counter reading-id)
    (ok (map-set quality-readings
      { reading-id: reading-id }
      {
        watershed-id: watershed-id,
        timestamp: stacks-block-height,
        validator: tx-sender,
        ph-level: ph,
        dissolved-oxygen: oxygen,
        turbidity: turbidity,
        temperature: temperature,
        nitrate-level: nitrates,
        phosphate-level: phosphates,
        bacterial-count: bacteria,
        heavy-metals: metals,
        verified: false,
        verification-count: u0
      }))))

(define-public (verify-quality-reading (reading-id uint))
  (let ((reading (unwrap! (map-get? quality-readings { reading-id: reading-id }) ERR-NOT-FOUND))
        (validator-info (unwrap! (map-get? authorized-validators { validator: tx-sender }) ERR-UNAUTHORIZED)))

    (asserts! (get active validator-info) ERR-UNAUTHORIZED)
    (asserts! (not (is-eq (get validator reading) tx-sender)) ERR-UNAUTHORIZED) ;; Can't verify own reading

    (ok (map-set quality-readings
      { reading-id: reading-id }
      (merge reading {
        verification-count: (+ (get verification-count reading) u1),
        verified: (>= (+ (get verification-count reading) u1) u2)
      })))))

(define-public (report-pollution-source
    (watershed-id uint)
    (pollution-type (string-ascii 50))
    (severity uint)
    (lat int)
    (lng int)
    (description (string-ascii 500)))
  (let ((source-id (+ (var-get source-counter) u1)))

    (asserts! (is-some (map-get? watersheds { watershed-id: watershed-id })) ERR-NOT-FOUND)
    (asserts! (and (>= severity u1) (<= severity u10)) ERR-INVALID-PARAMS)
    (asserts! (> (len description) u0) ERR-INVALID-PARAMS)

    (var-set source-counter source-id)
    (ok (map-set pollution-sources
      { source-id: source-id }
      {
        watershed-id: watershed-id,
        type: pollution-type,
        severity: severity,
        coordinates: { lat: lat, lng: lng },
        identified-by: tx-sender,
        timestamp: stacks-block-height,
        status: "active",
        description: description
      }))))

(define-public (update-pollution-status (source-id uint) (new-status (string-ascii 20)))
  (let ((source (unwrap! (map-get? pollution-sources { source-id: source-id }) ERR-NOT-FOUND)))

    (asserts! (or (is-eq tx-sender (get identified-by source))
                  (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)

    (ok (map-set pollution-sources
      { source-id: source-id }
      (merge source { status: new-status })))))

(define-public (assess-ecosystem-health
    (watershed-id uint)
    (biodiversity uint)
    (habitat-quality uint)
    (species-count uint)
    (invasive-present bool)
    (vegetation uint)
    (erosion uint)
    (indigenous-indicators (optional (string-ascii 200))))
  (let ((assessment-date (/ stacks-block-height u144))) ;; Daily assessments

    (asserts! (is-some (map-get? watersheds { watershed-id: watershed-id })) ERR-NOT-FOUND)
    (asserts! (<= biodiversity u1000) ERR-INVALID-PARAMS)
    (asserts! (<= habitat-quality u100) ERR-INVALID-PARAMS)
    (asserts! (and (>= erosion u1) (<= erosion u10)) ERR-INVALID-PARAMS)

    (ok (map-set ecosystem-health
      { watershed-id: watershed-id, assessment-date: assessment-date }
      {
        biodiversity-index: biodiversity,
        habitat-quality: habitat-quality,
        species-count: species-count,
        invasive-species-present: invasive-present,
        vegetation-coverage: vegetation,
        erosion-level: erosion,
        assessed-by: tx-sender,
        indigenous-indicators: indigenous-indicators
      }))))

(define-public (track-usage
    (watershed-id uint)
    (usage-type (string-ascii 30))
    (volume uint)
    (sustainability uint))
  (let ((usage-id (+ (var-get usage-counter) u1)))

    (asserts! (is-some (map-get? watersheds { watershed-id: watershed-id })) ERR-NOT-FOUND)
    (asserts! (> volume u0) ERR-INVALID-PARAMS)
    (asserts! (and (>= sustainability u1) (<= sustainability u10)) ERR-INVALID-PARAMS)

    (var-set usage-counter usage-id)
    (ok (map-set usage-tracking
      { usage-id: usage-id }
      {
        watershed-id: watershed-id,
        usage-type: usage-type,
        volume-liters: volume,
        date: stacks-block-height,
        reported-by: tx-sender,
        sustainable-rating: sustainability
      }))))

;; Read-only functions
(define-read-only (get-watershed (watershed-id uint))
  (map-get? watersheds { watershed-id: watershed-id }))

(define-read-only (get-latest-quality-reading (watershed-id uint))
  (let ((current-id (var-get reading-counter)))
    ;; In production, this would use a more efficient lookup
    (map-get? quality-readings { reading-id: current-id })))

(define-read-only (get-pollution-source (source-id uint))
  (map-get? pollution-sources { source-id: source-id }))

(define-read-only (get-ecosystem-assessment (watershed-id uint))
  (let ((current-date (/ stacks-block-height u144)))
    (map-get? ecosystem-health { watershed-id: watershed-id, assessment-date: current-date })))

(define-read-only (is-authorized-validator (validator principal))
  (match (map-get? authorized-validators { validator: validator })
    validator-info (get active validator-info)
    false))

(define-read-only (calculate-water-quality-index (reading-id uint))
  (match (map-get? quality-readings { reading-id: reading-id })
    reading (let ((ph-score (if (and (>= (get ph-level reading) u650) (<= (get ph-level reading) u850)) u100 u50))
                  (oxygen-score (if (>= (get dissolved-oxygen reading) u500) u100 u70))
                  (bacteria-score (if (<= (get bacterial-count reading) u100) u100 u30)))
              (some (/ (+ ph-score oxygen-score bacteria-score) u3)))
    none))

;; =====================================================
;; CONTRACT 2: community-stewardship.clar
;; Community coordination, education, and project management
;; =====================================================

;; Contract: community-stewardship
;; Purpose: Manages community projects, education, and indigenous knowledge integration

;; Contract constants
(define-constant MIN-PROJECT-STAKE u500000) ;; 0.5 STX
(define-constant EDUCATION-REWARD u100000) ;; 0.1 STX

;; Data structures
(define-map conservation-projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    watershed-id: uint,
    description: (string-ascii 1000),
    project-type: (string-ascii 50), ;; restoration, monitoring, education, infrastructure
    leader: principal,
    participants: (list 50 principal),
    funding-goal: uint,
    funds-raised: uint,
    start-date: uint,
    estimated-duration: uint,
    status: (string-ascii 20), ;; planning, active, completed, suspended
    impact-metrics: (optional (string-ascii 500))
  }
)

(define-map project-contributions
  { contributor: principal, project-id: uint }
  {
    amount: uint,
    contribution-type: (string-ascii 30), ;; funding, volunteer, expertise
    timestamp: uint,
    recognized: bool
  }
)

(define-map education-content
  { content-id: uint }
  {
    title: (string-ascii 200),
    content-type: (string-ascii 30), ;; article, video, workshop, traditional-knowledge
    author: principal,
    watershed-focus: (optional uint),
    indigenous-knowledge: bool,
    knowledge-keeper: (optional principal),
    completion-rewards: uint,
    completions: uint,
    created-at: uint,
    verified: bool
  }
)

(define-map learning-progress
  { learner: principal, content-id: uint }
  {
    started-at: uint,
    completed-at: (optional uint),
    progress-percentage: uint,
    quiz-score: (optional uint),
    certificate-earned: bool
  }
)

(define-map indigenous-knowledge-keepers
  { keeper: principal }
  {
    tribal-affiliation: (string-ascii 100),
    specialization: (list 5 (string-ascii 50)),
    verified-by-community: bool,
    knowledge-contributions: uint,
    respect-tokens: uint,
    active: bool
  }
)

(define-map community-proposals
  { proposal-id: uint }
  {
    title: (string-ascii 200),
    description: (string-ascii 1000),
    proposer: principal,
    proposal-type: (string-ascii 50), ;; policy, project, education, research
    voting-ends: uint,
    votes-for: uint,
    votes-against: uint,
    total-voters: uint,
    status: (string-ascii 20), ;; active, passed, rejected, executed
    execution-deadline: (optional uint)
  }
)

(define-map community-votes
  { voter: principal, proposal-id: uint }
  {
    vote: bool, ;; true = for, false = against
    voting-power: uint,
    timestamp: uint
  }
)

(define-map stewardship-badges
  { user: principal, badge-type: (string-ascii 50) }
  {
    earned-at: uint,
    criteria-met: (string-ascii 200),
    verified-by: principal,
    level: uint ;; 1-5 scale
  }
)

;; Counters
(define-data-var project-counter uint u0)
(define-data-var content-counter uint u0)
(define-data-var proposal-counter uint u0)

;; Community treasury
(define-data-var community-treasury uint u0)

;; Project management functions
(define-public (create-conservation-project
    (name (string-ascii 100))
    (watershed-id uint)
    (description (string-ascii 1000))
    (project-type (string-ascii 50))
    (funding-goal uint)
    (duration uint))
  (let ((project-id (+ (var-get project-counter) u1)))

    (asserts! (> (len name) u0) ERR-INVALID-PARAMS)
    (asserts! (> (len description) u10) ERR-INVALID-PARAMS)
    (asserts! (> funding-goal u0) ERR-INVALID-PARAMS)

    ;; Stake requirement for project creation
    (try! (stx-transfer? MIN-PROJECT-STAKE tx-sender (as-contract tx-sender)))

    (var-set project-counter project-id)
    (ok (map-set conservation-projects
      { project-id: project-id }
      {
        name: name,
        watershed-id: watershed-id,
        description: description,
        project-type: project-type,
        leader: tx-sender,
        participants: (list tx-sender),
        funding-goal: funding-goal,
        funds-raised: u0,
        start-date: stacks-block-height,
        estimated-duration: duration,
        status: "planning",
        impact-metrics: none
      }))))

(define-public (contribute-to-project (project-id uint) (amount uint))
  (let ((project (unwrap! (map-get? conservation-projects { project-id: project-id }) ERR-NOT-FOUND)))

    (asserts! (> amount u0) ERR-INVALID-PARAMS)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
    (asserts! (is-eq (get status project) "planning") ERR-INVALID-PARAMS)

    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Record contribution
    (map-set project-contributions
      { contributor: tx-sender, project-id: project-id }
      {
        amount: amount,
        contribution-type: "funding",
        timestamp: stacks-block-height,
        recognized: false
      })

    ;; Update project funding
    (ok (map-set conservation-projects
      { project-id: project-id }
      (merge project {
        funds-raised: (+ (get funds-raised project) amount)
      })))))

(define-public (join-project (project-id uint))
  (let ((project (unwrap! (map-get? conservation-projects { project-id: project-id }) ERR-NOT-FOUND)))

    (asserts! (not (is-some (index-of (get participants project) tx-sender))) ERR-ALREADY-EXISTS)
    (asserts! (< (len (get participants project)) u50) ERR-INVALID-PARAMS)

    (ok (map-set conservation-projects
      { project-id: project-id }
      (merge project {
        participants: (unwrap! (as-max-len? (append (get participants project) tx-sender) u50) ERR-INVALID-PARAMS)
      })))))

(define-public (update-project-status (project-id uint) (new-status (string-ascii 20)) (impact-metrics (optional (string-ascii 500))))
  (let ((project (unwrap! (map-get? conservation-projects { project-id: project-id }) ERR-NOT-FOUND)))

    (asserts! (is-eq tx-sender (get leader project)) ERR-UNAUTHORIZED)

    (ok (map-set conservation-projects
      { project-id: project-id }
      (merge project {
        status: new-status,
        impact-metrics: impact-metrics
      })))))

;; Education and knowledge management
(define-public (create-education-content
    (title (string-ascii 200))
    (content-type (string-ascii 30))
    (watershed-focus (optional uint))
    (is-indigenous bool)
    (knowledge-keeper (optional principal))
    (reward-amount uint))
  (let ((content-id (+ (var-get content-counter) u1)))

    (asserts! (> (len title) u0) ERR-INVALID-PARAMS)

    ;; If indigenous knowledge, verify keeper
    (if is-indigenous
      (begin
        (asserts! (is-some knowledge-keeper) ERR-INVALID-PARAMS)
        (asserts! (is-some (map-get? indigenous-knowledge-keepers { keeper: (unwrap-panic knowledge-keeper) })) ERR-UNAUTHORIZED))
      true)

    (var-set content-counter content-id)
    (ok (map-set education-content
      { content-id: content-id }
      {
        title: title,
        content-type: content-type,
        author: tx-sender,
        watershed-focus: watershed-focus,
        indigenous-knowledge: is-indigenous,
        knowledge-keeper: knowledge-keeper,
        completion-rewards: reward-amount,
        completions: u0,
        created-at: stacks-block-height,
        verified: false
      }))))

(define-public (register-knowledge-keeper
    (tribal-affiliation (string-ascii 100))
    (specializations (list 5 (string-ascii 50))))
  (begin
    (asserts! (is-none (map-get? indigenous-knowledge-keepers { keeper: tx-sender })) ERR-ALREADY-EXISTS)
    (asserts! (> (len tribal-affiliation) u0) ERR-INVALID-PARAMS)

    (ok (map-set indigenous-knowledge-keepers
      { keeper: tx-sender }
      {
        tribal-affiliation: tribal-affiliation,
        specialization: specializations,
        verified-by-community: false,
        knowledge-contributions: u0,
        respect-tokens: u0,
        active: true
      }))))

(define-public (complete-learning-content (content-id uint) (quiz-score (optional uint)))
  (let ((content (unwrap! (map-get? education-content { content-id: content-id }) ERR-NOT-FOUND)))

    (asserts! (is-none (map-get? learning-progress { learner: tx-sender, content-id: content-id })) ERR-ALREADY-EXISTS)

    ;; Award completion reward
    (if (> (get completion-rewards content) u0)
      (try! (as-contract (stx-transfer? (get completion-rewards content) tx-sender tx-sender)))
      true)

    ;; Record completion
    (map-set learning-progress
      { learner: tx-sender, content-id: content-id }
      {
        started-at: stacks-block-height,
        completed-at: (some stacks-block-height),
        progress-percentage: u100,
        quiz-score: quiz-score,
        certificate-earned: true
      })

    ;; Update content stats
    (ok (map-set education-content
      { content-id: content-id }
      (merge content {
        completions: (+ (get completions content) u1)
      })))))

;; Community governance
(define-public (create-proposal
    (title (string-ascii 200))
    (description (string-ascii 1000))
    (proposal-type (string-ascii 50))
    (voting-duration uint))
  (let ((proposal-id (+ (var-get proposal-counter) u1)))

    (asserts! (> (len title) u0) ERR-INVALID-PARAMS)
    (asserts! (> (len description) u10) ERR-INVALID-PARAMS)
    (asserts! (> voting-duration u0) ERR-INVALID-PARAMS)

    (var-set proposal-counter proposal-id)
    (ok (map-set community-proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        proposal-type: proposal-type,
        voting-ends: (+ stacks-block-height voting-duration),
        votes-for: u0,
        votes-against: u0,
        total-voters: u0,
        status: "active",
        execution-deadline: none
      }))))

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((proposal (unwrap! (map-get? community-proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
        (voting-power u1)) ;; Simple 1 vote per user, could be enhanced with reputation

    (asserts! (is-eq (get status proposal) "active") ERR-INVALID-PARAMS)
    (asserts! (< stacks-block-height (get voting-ends proposal)) ERR-INVALID-PARAMS)
    (asserts! (is-none (map-get? community-votes { voter: tx-sender, proposal-id: proposal-id })) ERR-ALREADY-EXISTS)

    ;; Record vote
    (map-set community-votes
      { voter: tx-sender, proposal-id: proposal-id }
      {
        vote: vote-for,
        voting-power: voting-power,
        timestamp: stacks-block-height
      })

    ;; Update proposal counts
    (ok (map-set community-proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for: (if vote-for (+ (get votes-for proposal) voting-power) (get votes-for proposal)),
        votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) voting-power)),
        total-voters: (+ (get total-voters proposal) u1)
      })))))

(define-public (award-stewardship-badge
    (recipient principal)
    (badge-type (string-ascii 50))
    (criteria (string-ascii 200))
    (level uint))
  (begin
    ;; Only contract owner or community leaders can award badges
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (and (>= level u1) (<= level u5)) ERR-INVALID-PARAMS)

    (ok (map-set stewardship-badges
      { user: recipient, badge-type: badge-type }
      {
        earned-at: stacks-block-height,
        criteria-met: criteria,
        verified-by: tx-sender,
        level: level
      }))))

;; Read-only functions
(define-read-only (get-project (project-id uint))
  (map-get? conservation-projects { project-id: project-id }))

(define-read-only (get-education-content (content-id uint))
  (map-get? education-content { content-id: content-id }))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? community-proposals { proposal-id: proposal-id }))

(define-read-only (get-learning-progress (learner principal) (content-id uint))
  (map-get? learning-progress { learner: learner, content-id: content-id }))

(define-read-only (is-knowledge-keeper (keeper principal))
  (match (map-get? indigenous-knowledge-keepers { keeper: keeper })
    keeper-info (and (get active keeper-info) (get verified-by-community keeper-info))
    false))

(define-read-only (get-user-badges (user principal))
  ;; In production, this would return a list of all badges for the user
  ;; This is simplified for the example
  (map-get? stewardship-badges { user: user, badge-type: "water-guardian" }))

(define-read-only (calculate-community-impact (watershed-id uint))
  ;; Calculate overall community impact score for a watershed
  ;; This would aggregate data from projects, education completions, etc.
  ;; Returns a score from 0-1000
  (some u750)) ;; Placeholder implementation
