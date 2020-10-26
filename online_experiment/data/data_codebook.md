Data Codebook for Online Experiment Data
========================================

The following describes the datasets contained in this directory. Taken together, they provide the data needed to reproduce the findings of the online experiment presented in
[Cascino et al. (2020): The Usefulness of Financial Accounting Information: Evidence from the Field](href=https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3008083)

### `online_exp_emails_sent.csv`

Each row contains data on one email that has been sent.

- `dtime`, format: YYYY-MM-DD HH:MM:SS in CET. Time when email was sent. 
- `wave`, format: categorical string. The emails included six different links that were used to differentiate responses into six groups:
  - `is_study`: Portfolio Manager, Rest of World (n = 5,255)
  - `is_survey`: Portfolio Manager, Europe (n = 5,376)
  - `is_task`: Portfolio Manager, North America	(n = 5,376) 
  - `study`: Financial Analysts, Rest of World (n = 12,110)
  - `survey`: Financial Analysts, Europe (n = 10,091)
  - `task`: Financial Analysts, North America (n = 21,351)
  

### `online_exp_line_items.csv`

These are the data to form the experimental income statement. They are used by the code for the experiment app and by the code to run the analyses. Each row is one income statement line item.

- `line_item`, format: identifying string. The title of the line item as displayed in the experimental material. 
- `var`, format: string. The variable name that is used to store response data for the respective line item (see `online_exp_response_data.csv`).
- `value`, format: integer. The value that was presented for the line item in the experimental material.
- `fixed`, format: Boolean (TRUE := 1). Whether or not the subjects could exclude the line item when constructing their earnings measure.


### `online_exp_response_data.csv`

These data contain responses from all 1,241 subjects that started the assignment by clicking on the email link. Each row contains the data for one participant.

- `id`, format: identifying string. An anonymous code identifying each participant.
- `occupation`, format: categorical string. Occupation of the respondent based on email wave: "Portfolio manager" or "Financial analyst".
- `location`, format: categorical string. Location of the respondent based on email wave: "Europe", "Rest of World" or "North America".
- `tstamp`, format YYYY-MM-DD HH:MM:SS in CET. Time when response data are last modified. As response data were updated on every respondent's action, these are available also for respondents that did not complete the assignment.  
- `treatment`, format: categorical integer. Summarizes the treatment that the respondent has received:
  - `1`: Strong corporate governance and no earnings management incentives
  - `2`: Strong corporate governance and earnings management incentives
  - `3`: Weak corporate governance and no earnings management incentives
  - `4`: Weak corporate governance and earnings management incentives
- `treated_cg`, format: Boolean. Whether the respondent has received the "weak corporate governance" treatment.
- `treated_em`, format: Boolean. Whether the respondent has received the "earnings management incentives" treatment.
- `did_mc`, format: Boolean. Whether the respondent has started the manipulation checks.
- `started_assignment`, format: Boolean. Whether the respondent has entered the assignment page of the app containing the income statement.
- `completed_assignment`, format: Boolean. Whether the respondent has submitted her/his modified income statement.
- `time_read`, format: numerical [seconds]. Time that the respondent spent between entering the experimental app and clicking on continue (to be taken to the manipulation checks).
- `time_check`, format: numerical [seconds]. Time that the respondent spent between entering the manipulation check page and submitting her/his manipulation check answers.
- `time_ready`, format: numerical [seconds]. Time that the respondent spent after receiving feedback on the manipulation checks and continuing to the assignment page.
- `time_assignment`, format: numerical [seconds]. Time that the respondent spent after entering the assignment page until submitting the modified income statement.
- `man_check_cgo_correct`, format: Boolean. Whether the respondent answered the question on the corporate governance manipulation correctly.
- `man_check_emi_correct`, format: Boolean. Whether the respondent answered the question on the earnings management manipulation correctly.
- `passed_both_mc`, format: Boolean. Whether the respondent answered both manipulation check questions correctly.
- `sga`, format: Boolean. Whether the respondent included the line item "Other selling, general and administrative expenses" in her/his adjusted earnings measure.
- `da`, format: Boolean. Whether the respondent included the line item "Depreciation and amortization" in her/his adjusted earnings measure.
- `fvg_market`, format: Boolean. Whether the respondent included the line item "Fair value gains on marketable securities (mark to market)" in her/his adjusted earnings measure.
- `fvg_model`, format: Boolean. Whether the respondent included the line item "Fair value gains on unlisted equity instruments (mark to model)" in her/his adjusted earnings measure.
- `gains_inv_property`, format: Boolean. Whether the respondent included the line item "Gains on revaluation of investment property" in her/his adjusted earnings measure.
- `int_exp`, format: Boolean. Whether the respondent included the line item "Interest expense" in her/his adjusted earnings measure.
- `tax_exp`, format: Boolean. Whether the respondent included the line item "Taxation expense"in her/his adjusted earnings measure.
- `dinclude`, format: numeric. The metric 'Delta Include' that is used as the main dependent variable in the paper. Delta Include is the percentage point difference between: (i) the average of the inclusion probabilities for the line items "Fair value gains on unlisted equity instruments (mark to model)" and "Gains on revaluation of investment property" and (ii) the inclusion probability of the line item "Fair value gains on marketable securities (mark to market)". Because our subjects can either include (1) or not (0) a line item, at the respondent level Delta Include can vary between -1 and 1 with feasible values being [-1, -0.5, 0, 0.5, 1]. 
