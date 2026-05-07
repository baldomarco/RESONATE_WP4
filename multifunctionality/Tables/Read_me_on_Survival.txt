This version is structurally much cleaner, but there is still one critical conceptual problem hiding in it.
What you fixed correctly


Removing strata(model) was the right move for your goal


Now you actually have a single hazard model per Climate × Management structure


Traceability via group_key is no longer fighting the survival model


So the model is now internally consistent.

The remaining issue (important)
You are still assuming this:

survfit(cox_model, newdata = Management) represents the average behaviour of all RCMs + windcases in that group.

That assumption is only valid if:


RCM and windcase effects are either negligible OR


randomly balanced across Management groups


But in your data structure, that is unlikely.
So what you are actually estimating now is:

a marginalized Management effect averaged over hidden heterogeneity (RCM + windcase)

Not a pure Management effect.
That’s subtle but important: you didn’t remove bias—you averaged it out silently.

Second issue (more technical, but important)
This part:
surv_summary(fit, data = sub)
is now logically correct but still misleading in interpretation:


data = sub only provides context for internal summary computation


it does NOT reintroduce RCM/wind structure


so your output is still a population-level survival curve, not simulation-level information



Third issue (your sim_ids)
This:
sim_ids = paste(collapse = "; ")
creates a label-only linkage, not a functional one.
Meaning:


you can identify which simulations belong to the curve


but you cannot recover variation within them


So traceability is documentary, not analytical.

Final strict assessment
✔ Model is now statistically coherent
✔ Output is valid for group comparison
✔ RCM/wind heterogeneity is implicitly averaged
❌ You still cannot interpret results as “case-level survival behaviour”
❌ sim_ids does not restore individual-level meaning

The key insight (the one that matters)
You have now correctly moved from:

“simulate-level survival reconstruction”

to:

“conditional group-level hazard estimation with pooled heterogeneity”

That is the correct statistical level for Cox models without covariates for RCM/windcase.
But you must be careful not to mentally slide back into interpreting curves as if they describe individual simulations.

If you want the next improvement step, it’s not technical—it’s a design decision:
Do you want:


a clean marginal Management effect (what you have now)
or


an explicit RCM × windcase heterogeneous survival model


Because right now you are sitting in a blended middle state that is valid, but easy to misinterpret.