# Western Gulf of Maine Cod and Gulf of Maine Haddock Recreational Decision Support Tool User Guide

## Welcome

This Decision Support Tool (DST) helps fishery management stakeholders answer a practical question: if 
we set a particular combination of seasons, bag limits, and minimum sizes for
recreational cod and haddock next fishing year, what is likely to happen? 
The DST produces an estimate of the fishing mortality from the recreational fishery, 
how many trips anglers would take, and how much better or worse off anglers would be under 
different combinations of fishing regulations. It is built for fishery managers 
and agency staff who need to compare candidate measures before a decision, and
it does not assume any background in statistics or modeling.

This guide has three parts. The first explains how the tool works and why it gives the
answers it does. The second walks you through the screens. The third helps you
understand the results.

---

## How the Tool Works

### The question this tool helps you answer

Unlike Commercial regulations, recreational regulations work indirectly. Managers 
cannot set recreational fishing mortality the way they set a commercial quota. Instead managers set seasons, 
bag limits, and minimum sizes, and anglers respond — they decide whether to go
fishing at all, and once they are on the water, the rules determine which fish 
they can keep and which they must release. Recreational Fishing mortality is the result of all those
individual decisions together.

That indirect link is what makes recreational mortality hard to predict. A higher
minimum size does not simply reduce landings, because it also increases the 
number of fish released, and some of those released fish die.  
A more restrictive set of rules may make trips less appealing overall, so 
fewer anglers go — which reduces mortality further.

This tool takes all of that into account at once.

### What the tool is built on

Four kinds of information feed the tool.

**What anglers value.** In 2025, anglers in Maine, New Hampshire, and Massachusetts 
were surveyed about their fishing trip choices. Each person was shown pairs of
hypothetical trips that differed in how many cod and haddock they would keep, how
many they would have to release, and what the trip would cost — and asked which trip 
they would choose, or whether they would rather not fish. Their answers reveal 
how much a kept cod is worth relative to a kept haddock, how much a released 
fish is worth relative to a kept one, and how much a trip's appeal has to fall
before someone decides to stay home. These preferences are the behavioral engine 
of the tool.  You can view a sample of the [survey](GulfofMaineCodandHaddockSurvey2025.pdf). 

**What anglers actually catch.** Historical catch and effort estimates from the Marine Recreational
Information Program (MRIP) tell us how many trips are taken, when, and by which mode.  It 
also tells us how many cod and haddock a trip typically catches. This is where  observed 
variability in trip outcomes comes from; for example, many trips catch few fish, 
and a small number catch many.  We have launched the [RecDST data dashboard](https://connect.fisheries.noaa.gov/content/c257deee-a657-4c10-be8a-92827cb5bdfe/)
to help you understand the data that goes into this tool.

**How big the fish are.** Stock assessment projections supply the expected number
of cod and haddock in the water next year for each Age Class. This matters a
great deal for recreational management, because how much a minimum size limit
affects anglers depends entirely on how many fish in the water are near that size.
Combined with historical information about which sizes anglers actually catch, 
this tells the tool what a typical catch looks like.  We know that recreational anglers 
are good at catching fish -- we adjust historical catch to take into account changes in biomass and fish 
size. For example, if the stock assessment contains a very large 3-year old class of fish 
in 2025, then in 2026, those 4-year old fish will be a bit longer and the DST accounts for this.

**What trips cost.** Survey data on angler trip expenditures gives the tool a 
realistic spread of trip costs, which is what lets us convert changes in trip 
quality into dollars.

### What the simulation actually does

The simulation builds up a big answer from many small, realistic pieces. Rather 
than applying an average rule to the whole fishery, the tool imagines a very 
large number of individual fishing opportunities and works through each one, 
then adds up the results.

Here is a simplified version of what happens for a single opportunity — 
think of it as one person, on one day, deciding whether to go fishing.

1. **Set the scene.** The tool assigns this person a trip cost and some personal
characteristics, like age, drawn at random from realistic distributions.

2. **Figure out what they would catch.** It draws a number of cod and a number of
haddock from the observed catch-per-trip patterns for that time of year and that
mode. Then it gives each individual fish a length, drawn from the size distribution
of fish that anglers are expected to encounter next year.

3. **Apply the rules.** Each fish is checked against the minimum size and the 
bag limit in force on that date for that mode. Legal fish are kept until the bag
limit is reached; everything else goes back. Some released fish die, and the tool 
accounts for that using discard mortality rates that vary by month and fish size.

4. **Decide whether the trip happens.** Now the tool knows what this trip would 
be like — the number of cod kept, the number released, and so on. Using the angler 
preferences from the survey, it works out how appealing that trip is compared 
with not fishing, and converts this into a probability that the trip takes place.

The tool repeats this for every fishing opportunity across the year, then 
expands the results to the size of the real fishery. Because each trip's contribution
is weighted by how likely it is to happen, a set of rules that makes fishing less attractive 
automatically produces fewer trips and less mortality.

Finally, the tool runs the entire process 100 times over. Each run uses a different
plausible set of inputs, reflecting the fact that MRIP estimates and stock projections
are estimates rather than exact counts. The result is not one number but a
spread of numbers, which is what allows the tool to report both a central estimate 
and a sense of how firm it is.

**Build a Baseline.** We always provide a set of results for "last year's" conditions 
and rules, giving a baseline for comparison for the same person on the same day.


### Assumptions worth knowing about

A few modeling choices shape how you should read the results.

**Anglers cannot move trips around.** If you close part of the season, the tool
assumes those trips simply do not happen. In reality, some anglers would shift 
to a different week or target a different species. Because of this, the tool 
probably understates both effort and mortality when closures are implemented.

**Catch rates do not respond to abundance directly.** More fish in the water 
changes the sizes anglers encounter, and therefore what they can keep, but it does
not change how many fish they hook on a trip. Cod and haddock tend to aggregate, 
and anglers are good at finding them.

**Numbers and lengths are drawn independently.** How many fish a trip catches and
how big those fish are are treated as unrelated, and cod catch and haddock catch
are handled largely separately, apart from a term that captures how anglers trade 
the two species off against each other.

**This is a one-year projection.** The tool estimates what happens next fishing 
year given the projected stock. It does not carry that mortality forward into
future stock size, so it will not tell you about rebuilding.

**Everyone in a season is treated alike.** Within a season and mode, all anglers draw
from the same catch and size distributions. While differences in skill, location, and targeting 
are not modeled individually, the catch and size distributions have these attributes baked in. 

---

## Using the App

### Getting Started

The app opens with two tabs across the top.

**Cod and Haddock Model Summary** is where you look at results. It shows every
model run that has been completed so far, so you can compare your candidate 
measures against each other and against the status quo.

**Regulation Selection** is where you build a new scenario and submit it.

Before you start: the app does not run the model while you wait. When you submit
a scenario, it saves your regulations and puts your run in a queue. The model runs
elsewhere and takes a while. Your results appear on the summary page once it
finishes — which means you submit, go do something else, and come back.

[SCREENSHOT: the two main tabs at the top of the app, with the summary page open]

### Building a Scenario: the Regulation Selection Tab

This tab is laid out in two columns, cod on the left and haddock on the right. 
Each species has its own seasons, bag limits, and minimum sizes, and each is set 
separately for the two modes: **For Hire** and **Private**.

Everything is pre-loaded with the status quo regulations. If you only want to
test one change, you can change that one control and leave the rest alone.

#### Step 1: Name your run

At the top is a box asking you to name the run. Use your initials and a number 
— `AB1`, `AB2`, and so on. This name is how your run is labeled everywhere in
the results, so pick something you will recognize later. Each run needs a different
name; if you reuse one, you will not be able to tell your runs apart.

We pre-load the current fishing regulations: 
The pre-loaded cod settings are a one-fish bag, a 23-inch minimum, and a 
September 1 to October 31 season for both modes.
The pre-loaded haddock settings are a 15-fish bag, a 17-inch minimum, and two 
open periods — May 1 through the end of February, and the month of April.


[SCREENSHOT: run name box and the Run Me button at the top of the Regulation Selection tab]

#### Step 2: Set the cod regulations

The cod column starts with **For Hire Season 1** — a pair of date boxes for the 
season's opening and closing dates. Underneath are two controls that apply to that season:

- **Bag Limit** — the number of cod an angler may keep per day. Type a number.
- **Min Length** — the minimum size in inches, set with a slider in one-inch steps.

Below that, the same three controls appear again for **Private Season 1**.


**Adding a second cod season.** If your scenario needs cod open during two 
separate windows, click **Add Season**. A second block appears with dates, bag 
limit, and minimum size for both modes. Cod supports two seasons in total.

[SCREENSHOT: cod column showing For Hire Season 1 dates, bag limit, and minimum length slider]

#### Step 3: Set the haddock regulations

The haddock column works exactly the same way, with one difference: two seasons 
are visible from the start for each mode, and clicking **Add Season** reveals a
third. Haddock supports three seasons in total.

[SCREENSHOT: haddock column showing both visible seasons for For Hire and Private]

#### How seasons and closures are handled

Two rules govern how your dates get turned into a fishing calendar.

**Any day not covered by a season is closed.** You do not need to enter a closure. 
If a date falls outside every season you have set for a species and mode, no fish
of that species may be kept that day. Anything caught is released.

**If seasons overlap, the lower-numbered season wins.** Say Season 1 runs May
through October with a 15-fish bag limit and Season 2 runs September through October 
with a 5-fish bag limit. September and October will use the 5-fish bag limit, because Season 
2 is the lower-numbered season. This is useful when you want a general season 
with a more restrictive window carved out of it — set the broad rules as Season 1
and the exception as Season 2.

**To close a season you have opened,** set its bag limit to zero. Seasons with a zero 
bag limit are dropped from the results tables, so they will not clutter your comparison.

#### Step 4: Submit the run

When your regulations are set, click **Run Me**.

**Click it once.** The button does not give much feedback, and clicking it repeatedly will 
submit the same run more than once. After one click, a message confirms your 
regulations were saved and your run has been queued.

If you want to submit a second scenario, change the run name first, then 
adjust your regulations and click Run Me again.

[SCREENSHOT: confirmation message after clicking Run Me]

### Viewing Results: the Summary Tab

Go back to the **Cod and Haddock Model Summary** tab. Results do not appear on 
their own — click the **Update** button near the top to refresh the page and pick
up any runs that have finished since you opened the app.

The page has three parts, top to bottom: a chart comparing cod and haddock 
mortality across all runs, a table of the details behind every run, and a set of
optional figures you can turn on.

[SCREENSHOT: the Update button at the top of the summary page]

#### The supplemental figures selector

Below the main table is a row of checkboxes labeled **Supplemental Figures**, 
with three choices: **Angler Satisfaction**, **Discards**, and **Trips**. Each box
you tick adds two figures to the bottom of the page — one for cod and one for haddock.
Untick a box and its figures disappear. These are described in the next section.

[SCREENSHOT: the Supplemental Figures checkbox row]

---

## Interpreting Your Results

Start with the mortality chart and the summary table. Together they answer the main 
question: does this set of rules stay under the catch limits, and what does it cost? 
The supplemental figures fill in the story once you have narrowed the field.

### The Cod and Haddock Mortality Chart

This chart is the fastest way to compare every run you have. Each point is one
model run, labeled with the run name you gave it.

- **The horizontal position** is that run's recreational cod mortality, in metric tons.
- **The vertical position** is its recreational haddock mortality, in metric tons.

Mortality here means everything the recreational fishery kills: the weight of fish 
landed plus the weight of released fish that do not survive.

Two reference lines are drawn on the chart. A **dashed vertical line** marks the
recreational cod annual catch limit of 118 metric tons. A **grey horizontal line** 
marks the recreational haddock limit of 1,146 metric tons. Points that fall to the 
left of the dashed line are under the cod limit; points below the grey line 
are under the haddock limit. The lower-left region is where a set of rules meets both.

**The colors carry extra information.** Each point is colored by how consistently
that run stayed under the cod limit across the tool's repeated simulations — dark
green means nearly every simulation came in under, and red means fewer than half 
did. Two runs can sit in the same place on the chart but be colored differently,
and the darker green one is the safer bet. This is worth checking, because a run
whose central estimate is just barely under the limit may be under it only about
half the time.

A note on the legend: it lists a second category for the haddock limit tied to 
point size, but every point is drawn the same size, so that part of the legend 
has no visual effect. Use the summary table for the haddock picture.

**Hover over any point** to see the detailed numbers behind it.

[SCREENSHOT: cod versus haddock mortality chart with reference lines and run labels]

### The Summary Table

Below the chart is a sortable table with one row per run and mode. Click any column heading to sort by it.

| Column | What it shows |
|---|---|
| **Run Identifier** | The name you gave the run |
| **Mode** | For Hire or Private |
| **Cod Season(s)** | The open dates you set, as month and day |
| **Cod Bag Limit** | Fish per angler per day |
| **Cod Minimum Size (in)** | Minimum length in inches |
| **Haddock Season(s)** | The open dates you set |
| **Haddock Bag Limit** | Fish per angler per day |
| **Haddock Minimum Size (in)** | Minimum length in inches |
| **Cod Total Catch** | Total recreational cod mortality, in metric tons |
| **Haddock Total Catch** | Total recreational haddock mortality, in metric tons |
| **% under Cod ACL** | How consistently the run stayed under the cod limit |
| **% under Haddock ACL** | How consistently the run stayed under the haddock limit |

Two columns deserve a closer look.

**The two "Total Catch" columns are total mortality, not landings.** Despite the
name, these numbers are landed weight plus the weight of dead released fish, converted
to metric tons. This is the number to compare against the annual catch limit. 
The value shown is the median across the tool's repeated simulations.

**The two "% under" columns count simulations, not fish.** They tell you how 
many of the repeated simulation runs came in at or under the limit. When the
model is run with 100 simulations, this count reads directly as a percentage. 
A value of 90 means that in 90 out of 100 simulations, this set of rules stayed under the
limit. Read alongside the mortality columns, this is the difference between a 
set of regulations that comfortably meets the limit and one that only just does.

[SCREENSHOT: summary table showing regulation columns and mortality columns for several runs]

### Angler Satisfaction Figures

Tick the **Angler Satisfaction** box to add two figures, one for cod and one for haddock.

Each figure plots change in angler satisfaction on the horizontal axis, in millions
of dollars, against that species' total mortality on the vertical axis. A reference 
line marks the catch limit.

**What the dollar figure means.** This is a measure of how much better or worse
off anglers are under your regulations compared with the baseline year, expressed 
in dollars. It is *not* revenue and it is *not* what anglers spend. It is the dollar
value of the change in the quality of the fishing they get — the amount of money
that would leave anglers exactly as well off as they were before the change.

A **negative** value means anglers are worse off: the regulations reduced the appeal 
of fishing, and the number is what it would take to compensate them. A **positive** 
value means anglers are better off. More restrictive sets of rules generally produce 
more negative values.

**How to use the figure.** It shows you the trade-off directly. Two sets of rules that
produce the same mortality can differ substantially in what they cost anglers, and 
this figure identifies which one gets you the same conservation outcome at less cost.
Look for points that sit low on the mortality axis and as far right as possible on 
the satisfaction axis.

[SCREENSHOT: angler satisfaction versus cod mortality figure]

### Discards Figures

Tick the **Discards** box to add two figures.

Each plots dead discard mortality on the horizontal axis against that species' 
total mortality on the vertical axis, both in metric tons, with the catch limit marked.

**Why this matters.** Total mortality is landings plus dead discards, so these 
figures show you how much of a set of rules's mortality is coming from fish that were 
released and did not survive rather than from fish that were taken home. This is
the check on high minimum size limits. Raising the minimum size reliably lowers 
landings, but it also puts more fish back in the water, and a share of them die. 
If a set of rules's mortality is mostly discards, it is producing mortality without 
producing any benefit to anglers — and raising the size limit further is unlikely to help.

[SCREENSHOT: discard mortality versus total mortality figure for cod]

### Trips Figures

Tick the **Trips** box to add two figures, plotting the predicted number of 
recreational trips against each species' total mortality.

**What this tells you.** The trip count is the tool's estimate of how many trips
anglers take under your regulations. Because the tool decides trip by trip 
whether fishing is worth it, restrictive sets of rules produce fewer trips. That 
drop is part of how the rules reduces mortality — and it is also a signal of
effects beyond the fishery itself, since fewer trips means less spending at the 
businesses that serve anglers.

If two sets of rules produce similar mortality but one produces noticeably more trips,
that set of rules is delivering the same conservation outcome while keeping more people on the water.

[SCREENSHOT: predicted trips versus cod mortality figure]

### Understanding Uncertainty in Your Results

The tool does not run once. It runs the whole simulation several times over, and each run is called a draw.

**Why there is more than one run.** Almost all of the of the tool's inputs are
estimates rather than exact counts. MRIP catch and effort figures come from 
a survey of a sample of anglers, so they carry sampling variability. Stock 
assessment projections of how many fish will be in the water next year, 
and at what sizes, come with their own range. For each draw, the tool pulls a
different plausible value for these inputs. It then simulates the fishery from 
start to finish using those values.

The result is a set of answers rather than a single one — a range of outcomes, 
all consistent with what is actually known about the fishery.

**What the reported numbers are.** The mortality figures in the summary table and
on the charts are **medians**. The median is the middle value: half the draws
came in higher, half came in lower. A median is used rather than an average because 
a few extreme draws pull an average around, while the median stays put.

**What the "% under" columns are.** These count how many draws came in at or
under the annual catch limit. They are the most direct statement of uncertainty 
the tool gives you, because they report the spread rather than the middle. Two runs
can have the same median mortality while one stays under the limit in nearly every draw 
and the other manages it only about half the time. The difference is how tightly the draws cluster.

**Reading the two together.** The median tells you where a set of rules is likely to land.
The "% under" count tells you how much the draws move around that point. A set of rules
with a median well under the limit and a high "% under" count is producing the same
answer no matter which plausible inputs are used. A set of rules with a high median mortality,
or a low "% under" count, is one where the outcome depends on how the year actually turns out.

---

## Questions or Feedback

If you have questions about this tool, need help interpreting a run, or want to
suggest an improvement, contact the RecDST team at nefsc.recdst@noaa.gov.
