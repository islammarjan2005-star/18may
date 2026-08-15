# Product Vision — Micro-Learning Inside the Scroll

**Working codename: OSMO**
*Studying, by osmosis.*

---

## 0. The one-line thesis

> The scarcest resource in learning is not time or intelligence. It is the willingness to *start*.
> So don't ask anyone to start. Put the studying inside something they already start, forty times a day, without thinking.

Everything below is downstream of that sentence. Where a decision is ambiguous, resolve it in favour of **lower activation energy**, even at the cost of pedagogical purity.

---

## 1. The strategic fork you have to resolve first

The concept as written says "a study card appears inside your Instagram feed." That literal product **cannot be built on iOS**, and is fragile-to-illegal everywhere else. This is the single most important constraint in the whole plan, so deal with it up front rather than discovering it in month four.

| Approach | Can it exist? | Verdict |
|---|---|---|
| **A. Inject cards into the real Instagram app feed** | No. iOS gives no app the ability to draw inside another app. Android could via an AccessibilityService overlay, but it's a Play Store policy minefield and breaks on every Instagram release. Also a straightforward ToS violation. | ❌ Not a business |
| **B. Browser extension for instagram.com / x.com / reddit.com** | Yes, genuinely — real injection into a real feed. But desktop web is a single-digit share of scroll time for students, and Meta throttles/obfuscates web DOM. | ✅ As a wedge and a demo, not as the product |
| **C. Interstitial / toll-booth: intercept the *moment of opening* Instagram** | Yes, legitimately, and via two different mechanisms — see §1.1. The **Shortcuts automation** route (what one sec, Clearspace and ScreenZen use) opens your app in full, giving you a complete interactive card. | ✅ **Strongest shippable hook** |
| **D. Own the scroll: your own short-form feed, cards interleaved** | Yes, entirely yours. Hard part is supplying content worth scrolling. | ✅ **The long game** |
| **E. Widgets, Lock Screen, Live Activities, keyboard, notification-inline answering** | Yes. Small surfaces, near-zero friction, no feed needed. | ✅ Cheap surface area, do it early |

### Recommendation: C → D, with B as marketing

Ship **C** first (the toll booth), because it captures the exact psychological moment the concept is about — *the instant of habit initiation* — without needing a single line of content supply. Then build **D**, your own feed, because that is where the durable product and the margins live. Use **B**, the extension, as a viral demo: a screen recording of a flashcard appearing between two Instagram posts is the best marketing asset this company will ever have, and it costs a week to build.

### 1.1 How the doorway is actually built (and what the wellbeing apps prove)

There are two implementations, and the less obvious one is much better:

- **Family Controls / `ManagedSettings` shield.** Blocks the app at launch, but the shield is *not* a free-form canvas — you control title, subtitle, icon and buttons only. The card would have to live one tap away.
- **Shortcuts personal automation** — *"When Instagram is opened → run [our app]."* This is what [one sec](https://one-sec.app/setup/), Clearspace and ScreenZen actually use, and it **launches your app in full**, so the card can be a complete interactive screen. This is the route to build on.

Three known costs of the Shortcuts route, all of which must be scoped into Phase 1:
1. **The infinite-loop problem.** Deep-linking back to `instagram://` re-fires the automation and drags the user straight back into your app. The fix is to expose a custom App Intent to Shortcuts and hold state in a shared App Group — solved, but non-trivial.
2. **Per-app setup in onboarding.** The user builds the automation by hand (guided). This is the single largest funnel drop in the wellbeing category and we inherit it. Invest in the guided flow accordingly, and default to gating *one* app, not five.
3. **Browsers are not intercepted.** Safari → instagram.com bypasses it entirely. This is precisely the hole the extension (route B) fills.

**What the category proves, and the warning inside it.** one sec's [PNAS field study](https://www.pnas.org/doi/abs/10.1073/pnas.2213114120) (280 users, six weeks) found ~36% of intercepted opens were abandoned and open attempts fell ~37% by week six. That validates the interception mechanic at scale — but note their goal is the *inverse* of ours: they want the user to turn back, we want them to answer and pass through. Encouraging, in that a 4-second card is far less adversarial than a forced breathing exercise. But it is also a hard warning: **friction measurably reduces app-opens**, so an annoying card erodes our own trigger surface. The intensity dial (§5) is therefore structural, not a preference. Track app-opens-per-day as a guardrail: if gating Instagram makes users open Instagram less, our own retrieval volume falls with it.

Be precise in your own language, always: the product is not *inside* Instagram. The product is **the same shape as Instagram**, and it sits **at the doorway to** Instagram. Users will not care about the distinction — as long as the feeling is identical. Guard the feeling obsessively.

---

## 2. Why this works (the psychology, honestly)

Six mechanisms are doing the real work. Two of them are risks pretending to be features. Know which is which.

**1. Activation energy is the whole game (Fogg: B = MAP).**
Behaviour happens when Motivation, Ability and a Prompt coincide. Study apps fail because they demand motivation *at a moment the user chose for its absence of demands*. We invert it: motivation can be near-zero, because ability is near-infinite (one thumb, four seconds) and the prompt is delivered by a habit that fires itself. Nobody ever "decides" to study. The decision — the expensive part — is deleted from the loop.

**2. Habit nesting, not habit stacking.**
Standard advice is "after I do X, I will do Y." That still requires a new behaviour to be initiated. We nest *inside* the existing behaviour, in a slot the user already tolerates being occupied by something they didn't choose — an ad. The feed has trained a billion people to accept unrequested interruptions in exchange for the next post. We're buying that slot back and putting something useful in it.

**3. Retrieval practice is unusually robust to bad conditions.**
The testing effect (Roediger & Karpicke) survives distraction, brevity and low stakes far better than reading or highlighting does. A four-second retrieval attempt in a noisy environment is worth more than four minutes of rereading in a quiet one. This is the pedagogical claim the product genuinely can defend — and importantly, it's the *only* one it needs.

**4. Spacing is a free gift from the format.**
Scrolling is already distributed across the day in dozens of short bursts. That is textbook distributed practice, and it's the schedule an SRS engine would design if it could. Traditional study apps have to *beg* users to come back four times a day. Ours gets it for free.

**5. Variable-ratio reinforcement — borrowed, not built.**
The card's unpredictable arrival inherits the feed's own slot-machine pull. Combined with the **Zeigarnik effect** — an unanswered question opens a loop the brain wants closed — a flashcard is one of the very few "productive" content types that is *natively scroll-compatible*. An article isn't. A lecture isn't. A question is, because the reveal is the payoff and it arrives in under two seconds.

**6. The real product is emotional: guilt arbitrage.**
The average student's scrolling session ends in a small tax of self-reproach. We don't reduce the scrolling. We change what it *costs emotionally*. Same forty minutes, but the session now ends with "I did 23 cards" instead of "where did that go." That reframing — **scrolling without the aftertaste** — is the thing people will actually pay for and tell friends about. Positioning, pricing, retention and brand should all be built on it.

### The two risks wearing feature costumes

- **Interference.** Consolidation is hurt by what comes immediately after encoding — and what comes immediately after our card is five seconds of loud video. Mitigation is a design constraint, not an afterthought: place cards *before* content resumes, insert a ~600ms quiet beat after the reveal, and treat every in-feed review as lower-confidence evidence than a deliberate one (see §7, the scheduler).
- **Productivity theatre.** A product that makes people *feel* they're learning while measurably doing little is a bad product with good retention — the worst kind. Countermeasure: commit early to a real efficacy study, hold yourself to a *retention* metric (cards recalled at 30 days) rather than a *volume* metric, and never market an outcome you haven't measured.

---

## 3. The experience

### 3.1 The card

The card must read as a **post**, not a worksheet. Same corner radius, same edge-to-edge width, same typographic scale as the surrounding content. If a user's eye can classify it as "the study thing" from peripheral vision, they will pre-emptively scroll past it and the product is dead.

```
┌─────────────────────────────────┐
│  econ · micro          seen 4×  │   ← quiet metadata, one line
│                                 │
│  Why does a monopolist          │   ← the question, large, centred
│  produce where MR = MC          │      never more than ~15 words
│  rather than P = MC?            │
│                                 │
│      tap to reveal              │   ← fades after the first few cards
└─────────────────────────────────┘
```

**Rules the card lives by:**

- **Zero latency, always.** Cards are pre-generated and pre-rendered several deep. A spinner on a study card is a fatal defect, not a bug — it breaks the illusion that this is just more feed. Treat "time to interactive = 0ms" as a hard requirement with a test that fails the build.
- **≤ 15 words on the front.** If a card can't be read in one saccade it's the wrong card for this context. Long cards get filtered out of the feed and offered in Desk Mode (§5) instead.
- **Skippable by scrolling.** Never trap the user. A scrolled-past card is *information*, not failure — it means "not now," and the scheduler treats it as a soft defer, with no penalty and no guilt copy. The ability to ignore it is precisely what makes it non-threatening enough to keep in the feed.
- **One thumb, no precision.** Reveal = tap anywhere. Rate = swipe. Left = *missed it*, right = *got it*, and a longer right-swipe (or a held swipe) = *easy*. Three grades, borrowing muscle memory people already have from Stories and Tinder. Anki's four-button grid is an expert tool; it does not belong here.
- **Haptics carry the reward.** A light impact on reveal, a crisper one on "got it." No coins, no confetti, no mascot. The dopamine budget is spent on *feel*, not on decoration — decoration is what makes it look like an edtech app.
- **Median interaction: 4–6 seconds.** Instrument it. If p50 drifts above 8s, the cards are too hard, not the users too slow.

### 3.2 Card types (variety is what keeps it feed-like)

An unbroken run of effortful recall turns the feed into a test. Mix in cards that cost nothing, exactly the way a feed mixes photos, ads and Reels:

| Type | Effort | Notes |
|---|---|---|
| **Classic flashcard** | Medium | The backbone. |
| **Cloze deletion** | Low | Fastest possible retrieval; great for languages and definitions. |
| **Multiple choice** | Low | One tap, no reveal step. Best card for a *first* session and for cold users. |
| **Image occlusion** | Medium | Non-negotiable for medicine (anatomy, histology) and geography. |
| **Audio card** | Low | Works *while* watching video with sound off; the only card type that can be answered without stopping. Language learners' killer feature. |
| **Explainer card** | Zero | Just a fact, beautifully set. No question. Pure recognition. This is the "breather" card and it's what keeps the ratio tolerable. |
| **Meme card** | Zero | A joke that happens to be a mnemonic. The most shareable object the product can produce, and therefore your cheapest acquisition channel. |
| **Streak-saver card** | Trivial | One deliberately easy card at the end of a dry spell. Momentum tool, used sparingly and never announced as such. |

### 3.3 Where cards appear

- **The doorway (toll booth).** You open Instagram; one card appears first. Answer it, or dismiss it — dismissal always works, with no dark pattern, no countdown, no shame screen. Default: one card, once per app-open, max ~8/day.
- **Your own feed (Osmo Feed).** Cards interleaved at the user's chosen density into a scrollable feed of study-adjacent content: community cards, deck-mates' additions, meme cards, curated shorts.
- **Ambient surfaces.** Home-screen widget (one card, tap to answer), Lock Screen widget, notification with inline answer buttons (answer without unlocking), Apple Watch complication, and — the sleeper hit — a **custom keyboard** that shows a card above the keys while you're typing anywhere.

### 3.4 Feeling like a feed without owning any video

We will never have a video library, and we don't need one. Three ways an app with no content of its own gets the scroll feeling:

1. **Own the moment instead of the content.** The entire wellbeing category (one sec, Opal, ScreenZen, Clearspace, Jomo) runs a profitable subscription with a content library of exactly zero. That's route C, §1.1.
2. **Make the unit itself scrollable.** X, Reddit, Pinterest, Tumblr and Substack Notes are all compulsive without video. The loop comes from *variable reward + infinite supply + near-zero cost per item* — video is one way to hit that bar, not a requirement. Cards, meme cards, explainer cards and deck-mate activity clear the same bar. **This is our feed.**
3. **Borrow via official embeds.** YouTube IFrame player (Shorts included), TikTok Embed SDK / oEmbed, Reddit and RSS aggregation. Legal, but you must use their player, retain their branding, and not strip their ads — and some terms explicitly prohibit assembling autoplay feed surfaces from them. Acceptable as filler between cards; never a foundation, and never a dependency.

Anything involving scraping Instagram or re-hosting other people's video is out of scope permanently — not worth the legal exposure or the takedown risk.

### 3.5 A day

> **07:52** — Alarm off, phone in hand, Instagram tapped by reflex. A card: *"Cross-price elasticity — sign for substitutes?"* Positive. Swipe right. Instagram opens. Elapsed: 3 seconds. He does not experience this as studying.
>
> **11:20** — Between lectures. Six cards over four app-opens. One he misses; the answer is one line and he half-reads it. The app doesn't lecture him about it.
>
> **14:05** — Ten minutes in the Osmo Feed itself, because a friend's shared deck on price discrimination showed up and it's more interesting than his group chat.
>
> **19:30** — Meme card, actually funny, screenshots it to the group chat. Two friends install the app that week. This is the growth loop, and it costs nothing.
>
> **23:40** — Doomscroll, the bad kind. Eleven cards ride along. He'd have scrolled anyway.
>
> **Sunday** — A card in the feed that isn't a card: *"You recalled 287 things this week. 41 of them you'd forgotten a month ago. That's about 26 minutes of active recall — during time you were going to spend scrolling regardless."* No trophy, no rank. Just a receipt.

That last one is the retention engine. **Show people the value they got from time they had already written off.** Nothing else in this category can make that claim.

---

## 4. Onboarding — the make-or-break 90 seconds

Every flashcard product dies at the empty deck. The user arrives motivated, is asked to author content, and leaves. **The user must never see an empty state.** Non-negotiable sequence:

1. **"What are you studying?"** — one screen, big tappable chips (Medicine, Law, Economics, CS, Spanish, Japanese, …), then a curriculum picker where one exists (USMLE Step 1, UK A-Level AQA Economics, LSAT, HSK 4, AWS SAA).
2. **Instant deck.** A curated starter deck is already loaded. They answer their first card **before** any account creation. First retrieval inside 20 seconds of install.
3. **Three ways to get *their* content**, in order of magic:
   - **Camera → notes.** Point at a page of lecture notes or a textbook; get 15 cards in ten seconds. This is the demo that sells the product. Build it first, make it excellent.
   - **Import** Anki `.apkg`, Quizlet, CSV, Notion, PDF, lecture slides, a YouTube lecture URL.
   - **Community decks** by curriculum, ranked by how well cards perform across users (see §9 — this data is a moat).
4. **Set the dial.** One slider, plain language, no numbers: *Occasionally · Regularly · Exam mode.* Show a live preview of what that feels like.
5. **Connect the habit.** "Which app do you open most?" → Screen Time permission, framed honestly: *"We'll put one card in front of Instagram. You can always skip it."*

Account creation happens somewhere after the fourth card, never before the first.

---

## 5. Modes

- **Ambient** (default) — 1 card per app-open, ~5–10/day. The "I'm not really studying" mode. This is the front door and most users should live here for weeks.
- **Regular** — cards interleaved every ~8–12 posts in the Osmo Feed, plus doorway cards. ~30–60/day.
- **Exam mode** — set your exam date; intensity auto-ramps on an exponential curve, the scheduler front-loads weak material, and the app surfaces a countdown that is factual rather than threatening. Turns itself off the day after the exam, on its own. *(This is also the strongest paywall placement in the product.)*
- **Cram** — 15 minutes, everything due plus everything shaky, ordered by expected marginal gain. This is the escape hatch for the night before, and it should be honestly labelled as the least effective mode in the app.
- **Desk mode** — a conventional, focused SRS session for the days someone actually *does* want to sit down. Long cards, typed answers, keyboard shortcuts, full stats. Do not skip this: the concept's whole premise is that starting is hard, but on the days it isn't, losing that user to Anki is a wound. Own both states of the same person.
- **Passive / Hands-free** — audio-only cards through headphones while walking or on a commute. Answer by voice or a headphone squeeze.

**Personalisation beyond intensity:** quiet hours; per-subject weighting ("70% pharmacology this fortnight"); *card mood* (serious / playful / meme-heavy); which apps act as doorways; difficulty ceiling in-feed (hard cards get routed to Desk mode); and a genuine **Pause** — one tap, no negotiation, no "are you sure you want to lose your streak." Trust is worth more than the extra retention that dark pattern would buy.

---

## 6. Motivation and long-term engagement

The audience is *specifically* people who fail at conventional study consistency. Applying conventional streak pressure to them recreates the exact failure they came to escape. So:

**Do:**
- **Streaks with mercy.** Automatic freezes, a low bar to maintain (one card counts), and a streak that never breaks in a way that feels punitive. A streak is a mirror, not a debt.
- **The weekly receipt.** Value framed as *reclaimed time*, always in units of the habit: "23 minutes of recall bought with scroll time."
- **Retention curves as art.** A gorgeous, honest chart of what you'd have forgotten by now versus what you've kept. This is the "Spotify Wrapped" of the product — annual, shareable, and it markets itself.
- **Identity feedback.** "You've now recalled 4,000 things." Identity beats incentive for long-run behaviour.
- **Ambient social.** Friends' *activity*, not friends' *scores*: "Maya added 14 cards on renal physiology" appears in your feed and you can pull them into your own deck in one tap. Cohort decks per course, per class, per exam sitting.
- **Creators.** Let strong students and tutors publish decks with a revenue share. Their audiences become your distribution.

**Don't:**
- Leaderboards as a default. Competitive gamification is a stimulant with a short half-life, and it re-imports performance anxiety.
- Guilt notifications ("You haven't studied today 😢"). Every notification we send should be *a card*, answerable in place — never a summons.
- Anything that makes the app better when the user scrolls *more*. See §12.

---

## 7. The technical differentiator: an interruption-aware scheduler

Every SRS algorithm in existence — SM-2, FSRS, Leitner — assumes a **deliberate session**: quiet, attentive, honest self-grading. Ours will run in the noisiest possible environment. Naively bolting FSRS onto a feed will produce a scheduler that is systematically wrong, and users will feel it as "this app keeps asking me things I know."

Build the thing that doesn't exist: a scheduler whose model of a review includes **the conditions of retrieval**.

- Weight each review by *context confidence*: doorway card (rushed, one-shot) < in-feed card < Desk mode review.
- Use **response latency** as a grade signal, not just the self-rating. Latency is more honest than self-report, and in this environment self-report is especially unreliable.
- Treat a **skip** as a genuine third signal — distinct from a lapse and from a pass. Nothing in the literature has this data; you will, at enormous scale.
- Model **interference**: reviews immediately followed by high-arousal content should decay faster in the model until proven otherwise.
- **Select for the moment, not just for the due queue.** At 07:52 with 3 seconds of attention, serve a cloze. At 14:05 in an actual session, serve the hard one. Due-date is an input to card selection, not the whole of it.

Two years of this data is a moat no competitor can copy by shipping features, and it's a legitimate research contribution — which in turn buys credibility, press, and institutional sales.

---

## 8. Brand

**What it must not look like:** green, owls, gamified mascots, "learn!", rounded friendly everything. The category signals of edtech are exactly the signals that trigger "this is a chore."

**What it should look like:** a well-made consumer social app that happens to teach you things. Dark-first, near-black rather than pure black. One hot accent — I'd take an electric citrus or an acid green-yellow — used sparingly. Editorial serif for the question text (it makes a card feel *considered*, and it's the fastest way to look unlike Quizlet), clean grotesque for UI. Motion that's fast and slightly springy. Sound design: two sounds, both subtle, both perfect.

**Voice:** dry, self-aware, complicit. It knows you're doomscrolling and isn't going to make a thing of it. *"You were going to scroll anyway."* Never earnest, never scolding, never exclamation marks.

**Names** (all quickly checkable for domain/App Store conflicts):

| Name | Why | Risk |
|---|---|---|
| **Osmo** | "Learning by osmosis" *is* the pitch. Short, warm, verb-able. | Adjacent to Osmosis (med-ed) — check trademark class carefully. |
| **Drip** | Describes the mechanic; "I've been dripping econ all week" works. | Crowded name. |
| **Lull** | The lulls in your scroll. Quiet, distinctive, ownable. | Sounds passive. |
| **Interleave** | Precisely correct pedagogically. | Too clever for consumer. |
| **Between** | "Studying, between the posts." Great taglines fall out of it. | Hard to search for. |

**Recommendation: Osmo**, with *Studying, by osmosis* as the line. It states the mechanism and the promise in three words and carries zero academic weight.

**Taglines to test:** *Study without studying.* · *You were going to scroll anyway.* · *Doomscrolling, with side effects.* · *Turn the scroll into revision.*

---

## 9. Monetisation

**Free:** own decks, ~20 cards/day, doorway mode, one subject, basic stats. Generous enough that the habit forms — the habit is the asset; never charge before it exists.

**Pro — £4.99/mo, £29.99/yr, £69 lifetime** (student pricing baked in, not a promo):
unlimited cards · AI card generation (camera/PDF/YouTube) with a monthly quota · **Exam mode** · image occlusion · audio cards · all surfaces (widgets, watch, keyboard) · full retention analytics · multi-device sync.

Notes on the model:
- **Lifetime matters** for this audience. Students hate subscriptions and love one-time unlocks, and a £69 lifetime from a med student in year two is excellent cash at the exact moment you need it.
- **Exam Pass (£14.99, 30 days)** — a seasonal SKU that matches the real demand curve. Panic is a legitimate willingness-to-pay event, and a monthly sub is the wrong shape for it.
- **Creator marketplace** — paid decks, 70/30 split. Turns your best users into a supply *and* distribution engine. Phase 3.
- **Institutional (B2B2C)** — med schools, language schools, professional-cert providers. Site licences plus cohort analytics. High ACV, slow sales cycle, so don't start it before you have efficacy data (§13). Publishers (test-prep, textbook) as licensed deck partners is the faster version of the same revenue.
- **Never ads.** The entire premise is that we're the *good* interruption in an ad-saturated feed. One ad destroys the thing users are paying us for. This isn't ethics, it's product integrity — though it's also ethics.
- **Never sell behavioural data.** We will hold unusually intimate data (what you open, when, how tired you are). Making that a revenue line ends the company. Say so publicly and early; it's a differentiator against every free study app.

Rough shape: 3–6% free→paid on a consumer funnel this frictionless is achievable if Exam mode is the paywall; the LTV lever is not price, it's surviving the post-exam churn cliff (§12).

---

## 10. Competition and moat

| Who | Their strength | Our angle |
|---|---|---|
| **Anki** | Best scheduler, fanatical med community, free | Brutal UX, desktop-shaped, requires deliberate sessions. We are the mobile, zero-initiation front-end for the same people — and we *import their decks* on day one. |
| **Quizlet** | Enormous content library, brand | Ad-heavy, teen-skewed, declining, gated content. We're premium-feeling and content-neutral. |
| **Duolingo** | Owns habit formation, best-in-class | Single vertical, and its habit is one *it* had to build. We attach to a habit that already exists and is 10× bigger. Their playbook validates ours; it doesn't block it. |
| **Gizmo / Wisdolia / Brainscape** | Closest in spirit; gamified mobile flashcards | They still ask you to open a study app. That's the whole difference. |
| **RemNote / Mochi** | Notes + SRS, power users | Desktop, deliberate, high setup cost. |

**The four moats, in order of durability:**
1. **Habit adjacency.** Distribution *inside* an existing behaviour. Nearly impossible to copy without the same device-level integrations and the same design discipline.
2. **Interruption-aware scheduling data** (§7). Compounding, proprietary, and nobody else is collecting it.
3. **Curriculum-level content network effects.** Once the best UKMLA / AQA Econ / HSK deck lives here and is continuously improved by per-card performance data across thousands of learners, switching cost is real.
4. **Device surface area.** Screen Time integration, widgets, watch, keyboard, extension — individually small, collectively a year of fiddly work a fast follower must repeat.

---

## 11. Audiences and go-to-market

**Beachhead: medical students.** Highest stakes, highest willingness to pay, an *existing* flashcard culture (Anki is already load-bearing for them), enormous shared decks, and image occlusion gives us a feature they can't get in a slick mobile form anywhere. They also scroll a great deal and feel guilty about it in a very targeted way. Win this and you have credibility, revenue, and the best word-of-mouth density of any student population on earth.

**Then, in order:** language learners (largest TAM, cards are natively the right unit, audio mode shines) → law (LSAT/bar, high stakes, high WTP) → A-level and undergraduate economics/CS/biology (huge volume, low WTP, great for scale) → professional certifications (CFA, PMP, AWS — adults with money and a doomscrolling habit, and no shame about wanting a shortcut).

**Positioning:** we don't compete on *effectiveness*, we compete on *whether learning happens at all*. The honest comparison isn't "Osmo vs. an hour of Anki." It's **"Osmo vs. the forty minutes you actually spent on TikTok."** Say that out loud in the marketing; the honesty is disarming and it's also true.

**Channels:**
- **Short-form video is the native channel** — the product demo *is* a Reel. Screen-record a card sliding between two posts. This is the rare consumer product whose core mechanic is inherently a good piece of content.
- **Meme cards as an acquisition loop.** Every screenshot is an ad with a watermark.
- **Exam-season timing.** Ad and content spend should be a spiky curve aligned to USMLE, A-level, finals, HSK sittings. This is a seasonal business — plan cash accordingly.
- **Community-first:** r/medicalschoolanki, r/6thForm, med-school Discords, language-learning TikTok. Ship the Anki importer before you post there; they will ask immediately.
- **Campus ambassadors**, curriculum-specific SEO landing pages ("AQA A-Level Economics flashcards"), and creator partnerships with study-influencers who already have the audience.

---

## 12. Ethics, and the metric that keeps you honest

This product sits close to a genuinely bad version of itself: an app that makes compulsive scrolling feel virtuous and therefore increases it. That version would retain beautifully and would deserve to fail.

Two commitments, made publicly and enforced in the metrics review:

1. **We never optimise for more scrolling.** No notification that invites a scroll session. No reward for time in feed. If a shipped change increases users' total scroll time, it gets rolled back regardless of what it did to engagement.
2. **We publish our own north star:** *retrievals per hour of scroll*. If our number goes up because scroll hours went down, that's a win. Track total screen time as a **guardrail metric with a hard ceiling.**

**Metrics that matter** (note that DAU is nearly useless here — the app is fused to a habit that already fires daily):
- **Primary:** retrievals/user/day; % of app-opens containing ≥1 retrieval.
- **Learning:** 30-day retention rate of reviewed cards (the honesty metric).
- **Experience:** p50 card interaction time; skip rate by card type; card render latency (must be ~0).
- **Business:** free→Pro conversion, and above all **post-exam survival** — the fraction of users still active 30 days after their exam date. This is the number that decides whether you have a study tool or a company.
- **Guardrail:** total scroll time, per user, trending flat or down.

**Risk register:** platform dependency (Apple can change Family Controls; Meta can break the extension) → never let one surface be the only surface. Efficacy scepticism → commission a real study with a university partner by year two; it unlocks institutional sales and pre-empts the inevitable "does this actually work?" article. Privacy → all card content and scheduling on-device where possible, plain-language policy, no data sales, ever.

---

## 13. Roadmap

**Phase 0 — Prove the feeling (weeks 1–6).**
Browser extension injecting cards into instagram.com and x.com, plus a TestFlight app with the doorway mode. No accounts, no sync, one hard-coded deck. The only question: *do people answer the cards, or do they resent them?* Ship it to 100 med students and watch skip rate. If skip rate is above ~60% after week one, the interruption isn't welcome and the whole thesis needs rework — better to learn that in week six than year two.

Second question for Phase 0, equally decisive: **what does the app-switch cost?** The Shortcuts doorway (§1.1) gives you a full card, but it still bounces the user out of Instagram and back. Measure answer rate for (a) the extension's true in-feed card versus (b) the automation → card → return flow. Watch the **automation setup completion rate** in onboarding at the same time — if fewer than half of installs finish it, the doorway is a leaky front door and weight shifts to widgets, notifications and the owned feed.

**Phase 1 — MVP (months 2–4).**
iOS first. Doorway mode via Screen Time. Anki + Quizlet + CSV import. Three card types (basic, cloze, MCQ). Intensity dial. FSRS baseline scheduler. Home-screen widget. Weekly receipt. Free with a soft cap. **Goal: 20 retrievals/user/day, 40% week-4 retention.**

**Phase 2 — The product (months 4–8).**
Camera→cards AI generation (the demo that sells it). Osmo Feed with interleaving. Image occlusion and audio cards. Exam mode + the paywall. Desk mode. Android. Community decks by curriculum. **Goal: first £10k MRR; conversion ≥3%.**

**Phase 3 — The moat (months 8–16).**
Interruption-aware scheduler v1 on real data. Creator marketplace with rev share. Cohort/class decks and ambient social. Watch, Lock Screen, keyboard. Annual retention "Wrapped." Efficacy study kicked off. **Goal: 100k MAU, £100k MRR, one credible learning-outcome result.**

**Phase 4 — The category (year 2+).**
Institutional and publisher deals. Non-student verticals (professional certs, corporate compliance done humanely, onboarding). Open the doorway to other feeds and other host habits — the mechanism generalises far beyond Instagram, and far beyond studying. The long-run framing:

> **Osmo is not a study app. It's a way to put anything worth remembering inside a habit people already have.**
> Flashcards are simply the first, and most obviously valuable, thing to put there.

---

## 14. The five decisions to make this week

1. **Doorway-first, or feed-first?** (Recommendation: doorway. Cheaper, and it tests the psychology directly.)
2. **Beachhead: medicine or languages?** (Recommendation: medicine for revenue and credibility; languages second for scale.)
3. **Will you build Desk mode?** (Recommendation: yes, by Phase 2 — otherwise you hand your best users back to Anki on their most motivated days.)
4. **Free tier generosity.** (Recommendation: err generous. The habit is the asset; charge for exam panic, not for existence.)
5. **The public commitment on scroll time.** (Recommendation: make it, loudly. It's a real differentiator, it constrains you usefully, and it's the right thing to do.)
