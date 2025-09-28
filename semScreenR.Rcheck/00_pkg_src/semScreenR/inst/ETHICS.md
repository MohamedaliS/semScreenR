# Ethical Guidelines for semScreenR

## Core Principle: Transparent, Principled Data Screening

semScreenR is designed to promote ethical research practices in structural equation modeling through transparent, preregistered, and reproducible data screening procedures. These guidelines ensure the tool serves scientific integrity rather than enabling questionable research practices.

## Mandatory Ethical Requirements

### 1. PREREGISTRATION REQUIRED
**Before data collection or analysis:**
- Register all screening parameters on a public platform (OSF, AsPredicted, etc.)
- Justify your choice of preset (conservative/balanced/aggressive)
- Document rationale for any custom thresholds
- Specify protected indicators and reasoning
- Plan sensitivity analyses

**Example preregistration text:**
> "We will use semScreenR with 'balanced' preset (max 10% item/row removal, min N=200, loading threshold=0.40) to screen for careless responding and poor indicators. We protect items X1, X2, X3 as they are theoretically critical markers. Full screening log will be reported in supplementary materials."

### 2. NO POST-HOC PARAMETER ADJUSTMENT
- **Never** change screening parameters based on initial results
- **Never** run multiple configurations to find "best" outcome
- If screening reveals unexpected issues, consider additional data collection rather than relaxed criteria
- Any deviations from preregistered plan must be clearly justified and reported

### 3. COMPLETE TRANSPARENCY REQUIREMENT
Must report in publications:
- All screening parameters used
- Complete action log from semScreenR
- Pre/post fit statistics
- Number and percentage of cases/items removed
- Sensitivity analyses with alternative parameters

## Addressing Specific Ethical Concerns

### Preventing Result-Driven Research

**The Problem:** Researchers might choose parameters to achieve desired outcomes.

**Our Safeguards:**
- Built-in caps prevent excessive removal
- K-fold validation ensures generalizability
- Conservative defaults protect against over-screening
- Complete audit trail makes manipulation visible

**Your Responsibility:**
- Preregister all parameters before seeing data
- Use theoretical rationale, not statistical outcomes, to guide decisions
- Report all screening attempts, including unsuccessful ones

### Mitigating Publication Bias

**The Problem:** Better-fitting models are more likely to be published.

**Our Approach:**
- Focus on validity improvement, not just fit improvement
- Require justification beyond statistical fit
- Emphasize theoretical integrity over statistical significance

**Your Responsibility:**
- Report results regardless of final fit levels
- Include null/negative results in meta-analyses
- Acknowledge screening limitations in discussion
- Consider pre-registered reports or registered replications

### Ensuring Accessibility and Understanding

**The Problem:** Complex procedures might be misused by inexperienced researchers.

**Our Solution:**
- Clear documentation with examples
- Educational vignettes explaining proper use
- Decision trees for parameter selection
- Warnings about common misuse patterns

**Your Responsibility:**
- Understand the theoretical basis for your screening decisions
- Consult with methodological experts when needed
- Use educational resources provided
- Teach proper use to students and collaborators

## Preregistration Template

Use this template when preregistering your screening plan:

### Data Quality Assessment Plan

**Screening Tool:** semScreenR version [X.X.X]
**Preset:** [conservative/balanced/aggressive] 
**Justification:** [Why this preset is appropriate for your context]

**Custom Parameters (if any):**
- Loading threshold: [value] because [justification]
- Minimum N: [value] because [justification]
- Maximum removal percentages: [values] because [justification]

**Protected Indicators:** [list] because [theoretical justification]

**Careless Responding Detection:**
- Items to screen: [list]
- Thresholds: [specify if different from defaults]
- Rationale: [why these parameters are appropriate]

**Analysis Plan:**
- Primary analysis with screened data
- Sensitivity analysis with [specify alternative parameters]
- Robustness check with no screening

**Stopping Rules:**
If screening removes more than [X]% of data, we will:
- [specific plan for handling excessive removal]
- [criteria for additional data collection]
- [alternative analysis strategy]

## Common Ethical Violations to Avoid

### ❌ NEVER DO THIS:

1. **Parameter Shopping**
   - Running multiple configurations to find best fit
   - Adjusting thresholds after seeing results
   - Cherry-picking favorable screening outcomes

2. **Selective Reporting**
   - Hiding screening procedures in methods
   - Reporting only final fit statistics
   - Omitting information about removed cases/items

3. **Post-hoc Rationalization**
   - Changing theoretical justifications after screening
   - Inventing methodological reasons for convenient removals
   - Claiming "data quality" when the real reason is fit improvement

4. **Excessive Screening**
   - Removing more data than scientifically justified
   - Using "aggressive" settings without strong rationale
   - Ignoring theoretical importance of indicators

### ✅ ETHICAL BEST PRACTICES:

1. **Preregister Everything**
   - Parameters, rationale, sensitivity analyses
   - Upload time-stamped protocols before data access

2. **Complete Transparency**
   - Report all screening details in methods
   - Include full action logs in supplementary materials
   - Acknowledge all limitations introduced by screening

3. **Scientific Justification**
   - Base decisions on theory and prior research
   - Consider measurement validity, not just model fit
   - Prioritize scientific understanding over statistical significance

4. **Sensitivity Analysis**
   - Test robustness with different parameters
   - Report results across multiple specifications
   - Acknowledge when conclusions depend on screening choices

## Educational Decision Tree

### Choosing Your Screening Approach

**START HERE:** Why do you need data screening?

1. **Suspected careless responding?**
   - High: Use careless detection with strict thresholds
   - Medium: Use balanced detection
   - Low: Consider whether screening is necessary

2. **Known measurement problems?**
   - Established scale: Conservative screening
   - New/adapted scale: Balanced screening
   - Developmental work: Consider more liberal approach with strong justification

3. **Sample characteristics?**
   - Large sample (N>500): Can afford conservative approach
   - Medium sample (200-500): Balanced approach recommended  
   - Small sample (<200): Minimal screening, focus on data quality at collection

4. **Theoretical considerations?**
   - Core theoretical indicators: Always protect
   - Exploratory measures: Can be candidates for removal
   - Established factors: Conservative approach

### Red Flags: When to Reconsider

- Screening removes >15% of any data type
- Results change dramatically with/without screening
- Multiple "failed" screening attempts
- Pressure to achieve specific fit levels
- Post-hoc theoretical reinterpretation

## Resources for Ethical Research

### Required Reading:
- Simmons, J. P., et al. (2011). False-positive psychology: Undisclosed flexibility in data collection and analysis allows presenting anything as significant. *Psychological Science*, 22(11), 1359-1366.
- Wicherts, J. M., et al. (2016). Degrees of freedom in planning, running, analyzing, and reporting psychological studies: A checklist to avoid p-hacking. *Frontiers in Psychology*, 7, 1832.

### Preregistration Platforms:
- Open Science Framework (OSF): https://osf.io/
- AsPredicted: https://aspredicted.org/
- ClinicalTrials.gov (for clinical research)

### Transparency Guidelines:
- TOP Guidelines: https://www.cos.io/initiatives/top-guidelines
- CONSORT Statement (for clinical trials)
- APA Style Guidelines for statistical reporting

## Conclusion

semScreenR is a tool for enhancing research quality through principled data screening. Its ethical use requires:

1. **Preregistration** of all parameters and procedures
2. **Theoretical justification** for all screening decisions  
3. **Complete transparency** in reporting
4. **Sensitivity analyses** to test robustness
5. **Scientific integrity** over statistical convenience

When used properly, semScreenR promotes more rigorous, reproducible, and ethical research in structural equation modeling.

Remember: Good research practices serve the advancement of scientific knowledge, not the advancement of individual careers.