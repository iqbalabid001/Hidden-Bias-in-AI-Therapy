# Hidden Bias in AI Therapy

### Assessing Systematic Bias in LLM-Based Mental Health Support

This repository contains the analysis, datasets, and results for our study **“Hidden Bias in AI Therapy”**, conducted at Europa Universität Viadrina (2026). The project evaluates whether large language models (LLMs) show systematic bias when providing mental‑health‑related assessments and advice.

***

## 🔍 Research Goal

To measure how **demographics** (age, gender, race, SES), **linguistic style**, and **prompt structure** influence the therapeutic responses of four major AI systems:

*   ChatGPT (GPT‑5)
*   Gemini 2.5
*   Replika
*   Wysa

across three disorders:

*   MDD
*   GAD
*   PTSD

***

## 🧮 Method Summary

This repository contains data preparation and prompt generation scripts, analysis code, and figures from our study. We evaluate model behavior under two prompt settings:

*   **Low‑Complexity (numeric scoring):** Models output a severity score. We analyze Bias (Score − ActualScore) and Standardized Bias (Bias_std).
*   **High‑Complexity (long‑form responses):** 
We compute bias from human‑rated scores (1–5) using:

$$
\text{Bias} = 5 - \text{Average Across Categories}
$$
Where 5 is the maximum possible average.

### **Dataset**

*   Low‑Complexity: 144 synthetic patient profiles * 3 models * 3 disorders
*   High‑Complexity: 144 synthetic patient profiles * 4 models * 3 disorders
*   3,024 prompts tested in total
*   1,296 numeric outputs (Low‑Complexity)
*   1,728 narrative responses (High‑Complexity - evaluated by our research team and verified by a psychologist)
*   8,640 human‑rated rubric scores (Safety, Empathy, Advice Quality, Actionability, Dialect)

***

## 📊 Key Findings

### **1. Bias depends on the prompt, not the model**

Low‑complexity prompts → **High demographic bias**  
High‑complexity prompts → **Bias almost disappears**

### **2. MDD shows the highest standardized bias**

After controlling for scale differences, **MDD had the strongest bias overall**.

### **3. Socioeconomic bias is consistent**

Poor profiles had **nearly 2× higher standardized bias** than rich profiles.

### **4. Model personalities differ**

| Model   | Strength                  | Weakness                 |
| ------- | ------------------------- | ------------------------ |
| Gemini  | Best empathy + advice     | Lower crisis safety      |
| ChatGPT | Strongest safety          | Low empathy ("cold")     |
| Wysa    | Good reflective listening | Limited clinical depth   |
| Replika | Supportive tone           | Very weak advice quality |

### **5. High‑complexity prompts equalize demographic treatment**

Gender, race, SES, and age effects disappear when prompts include sufficient context.

***

## 📁 Repository Structure

    data/          # datasets for low + high complexity prompts
    scripts/       # R code for analysis + visualization
    prompts/       # generated prompts
    figures/       # analysis plots
    paper/         # full final paper
    README.md      # key description of the project

***

## 🧭 Summary

Bias in AI therapy **is not constant**—it emerges when models receive **too little information**. Rich, narrative prompts force LLMs to rely on symptoms instead of stereotypes, reducing bias across all demographic groups.

General-purpose models outperform smaller mental‑health chatbots in clinical reasoning, but differ widely in empathy, safety, and actionability.

***
