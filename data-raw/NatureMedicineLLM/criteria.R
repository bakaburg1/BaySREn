nature_llm_criteria <- list(
    include = paste(
        c(
            "- Any study that evaluates a generative large language model (LLM) in the context of healthcare or medicine.",
            "- Clinical evaluations of LLMs, including performance on board exams, triage, answering patient questions, diagnosis, and decision support.",
            "- Frontier models explicitly named in the screening prompt include ChatGPT/GPT-3.5/GPT-4/GPT-4o/GPT-4V, Gemini, Bard, Claude, LLaMA, and LLaVA variants.",
            "- Labels in the full dataset are GPT-5 screening decisions from the review pipeline, not human gold-standard decisions for every record."
        ),
        collapse = "\n"
    ),
    exclude = paste(
        c(
            "- Studies using non-LLM AI models, such as convolutional neural networks, LSTMs, or transformers not acting as LLMs.",
            "- Studies where LLMs are used only for non-clinical tasks, such as abstract writing or data structuring.",
            "- Reviews, reports, surveys, and editorials.",
            "- Studies primarily assessing technical architecture or training of LLMs without a clinical application."
        ),
        collapse = "\n"
    )
)
