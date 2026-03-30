from __future__ import annotations

import math
from collections import Counter
from io import BytesIO
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import numpy as np
import pandas as pd


# =========================================================
# IPA DEFINITIONS
# =========================================================

ipa_consonants = [
    'p', 'b', 't', 'd', 'ʈ', 'ɖ', 'c', 'ɟ', 'k', 'g', 'q', 'ɢ', 'ʔ', 'm', 'ɱ',
    'n', 'ɳ', 'ɲ', 'ŋ', 'ɴ', 'ʙ', 'r', 'ʀ', 'ɾ', 'ɽ', 'ʃ', 'ʒ', 'ʂ', 'ʐ', 'ç',
    'ʝ', 'x', 'ɣ', 'χ', 'ʁ', 'ħ', 'ʕ', 'h', 'ɦ', 'ɬ', 'ɮ', 'ʋ', 'ɹ', 'ɻ', 'j',
    'ɰ', 'ʍ', 'w', 'ɥ', 'ʜ', 'ʢ', 'ʡ', 'ɕ', 'ʑ', 'ɺ', 'ɧ', 'ð', 'θ', 'v', 'f',
    'z', 's', 'ʒ', 'ʃ', 'l', 'ɡ', 'ʤ', 'ʧ', 'ɭ'
]

ipa_vowels = [
    'i', 'y', 'ɨ', 'ʉ', 'ɯ', 'u', 'ɪ', 'ʏ', 'ʊ', 'e', 'ø', 'ɘ', 'ɵ', 'ɤ', 'o',
    'ə', 'ɛ', 'œ', 'ɜ', 'ɞ', 'ʌ', 'ɔ', 'æ', 'ɐ', 'a', 'ɶ', 'ä', 'ɑ', 'ɒ', 'ɚ',
    'ɝ', 'aɪ', 'aʊ', 'eɪ', 'oʊ', 'ɔɪ', 'ɪə', 'eə', 'ʊə', 'Å', 'Æ', 'Ç', 'È', 'É',
    'Î', 'Ï', 'Ð', 'Ñ', 'Ò', 'Ó', 'Ô'
]


# =========================================================
# DISPLAY RESTORATION MAPS
# =========================================================

FORWARD_REPLACEMENTS = {
    'aɪ': 'Å', 'ɑɪ': 'Å', 'ai': 'Å',
    'aʊ': 'Æ',
    'eɪ': 'Ç',
    'oʊ': 'È',
    'ɔɪ': 'É', 'ɔi': 'É', 'oɪ': 'É',
    'tʃ': 'ʧ',
    'dʒ': 'ʤ',
    'r': 'ɹ',
    'ʌ': 'ə',
    'ɝ': 'ɚ',
    'ɫ': 'l',
    'ʁ': 'ɹ',
    'ɡ': 'g',
    'ɔɹ': 'Î',
    'ɑɹ': 'Ï', 'aɹ': 'Ï',
    'ɛɹ': 'Ð',
    'oɹ': 'Ñ',
    'eɹ': 'Ò',
    'ɪɹ': 'Ó', 'iɹ': 'Ó',
    'ʊɹ': 'Ô',
    'əɹ': 'ɚ',
    'ɜɹ': 'ɚ',
}

REVERSE_REPLACEMENTS = {
    'Å': 'aɪ',
    'Æ': 'aʊ',
    'Ç': 'eɪ',
    'È': 'oʊ',
    'É': 'ɔɪ',
    'Î': 'ɔɹ',
    'Ï': 'ɑɹ',
    'Ð': 'ɛɹ',
    'Ñ': 'oɹ',
    'Ò': 'eɹ',
    'Ó': 'ɪɹ',
    'Ô': 'ʊɹ',
}


# =========================================================
# PUBLIC API
# =========================================================

ALGORITHMS = ("unweighted", "weighted", "PMI")


class LEAAPError(ValueError):
    """User-facing validation/analysis error suitable for Shiny messages."""


def analyze_file(
    input_file: str,
    selected_algorithms: Sequence[str] = ("unweighted", "weighted", "PMI"),
    include_phoneme_alignments: bool = True,
    include_full_alignments: bool = False,
    round_digits: int = 6,
) -> Dict[str, object]:
    """
    Main entry point for Shiny / reticulate.

    Parameters
    ----------
    input_file:
        Uploaded file path (.csv or .xlsx).
    selected_algorithms:
        Any combination of: 'unweighted', 'weighted', 'PMI'.
    include_phoneme_alignments:
        Whether to return the per-slot alignment dataframe.
    include_full_alignments:
        Whether to build the wide human-readable alignment display dataframe.
    round_digits:
        Number of decimal places for exported numeric values.

    Returns
    -------
    dict with:
        - item_error_counts: pd.DataFrame
        - phoneme_alignments: pd.DataFrame | None
        - full_alignments: pd.DataFrame | None
        - grouping_labels: list[str]
        - grouping_map: dict[str, str]
        - metadata: dict
    """
    algorithms = _normalize_algorithms(selected_algorithms)

    df_all, referent_df, comparator_df, grouping_cols, item_labels = load_input_data(input_file)
    grouping_labels, grouping_map = get_grouping_options_from_columns(grouping_cols)

    long_data = reshape_to_long(df_all, grouping_cols, item_labels)
    referent_long, comparator_long = split_long_by_role(long_data)
    referent_map = build_referent_map(referent_long)

    item_header = (
        ["comparator_speaker"]
        + grouping_cols
        + [
            "item_label",
            "referent_speaker",
            "referent_item",
            "comparator_item",
            "total_errors",
            "vowel_for_consonant",
            "consonant_for_vowel",
            "insertion",
            "deletion",
            "vowel_sub",
            "consonant_sub",
        ]
    )

    if "unweighted" in algorithms:
        item_header.append("unweighted_lev")
    if "weighted" in algorithms:
        item_header.append("weighted_lev")
    if "PMI" in algorithms:
        item_header.append("PMI_lev")

    phoneme_header = (
        ["comparator_speaker"]
        + grouping_cols
        + [
            "item_label",
            "referent_speaker",
            "referent_item",
            "comparator_item",
            "referent_phoneme",
            "comparator_phoneme",
            "error_type",
            "position",
            "referent_length",
        ]
    )

    if "unweighted" in algorithms:
        phoneme_header.append("unweighted_penalty")
    if "weighted" in algorithms:
        phoneme_header.append("weighted_penalty")
    if "PMI" in algorithms:
        phoneme_header.extend(["pmi_value", "pmi_penalty"])

    item_rows = [item_header]
    phoneme_rows = [phoneme_header]

    header = comparator_long[0]
    speaker_idx = header.index("speaker")
    item_label_idx = header.index("item_label")
    item_idx = header.index("item")

    for comparator in comparator_long[1:]:
        comparator_speaker = comparator[speaker_idx]
        item_label = comparator[item_label_idx]
        comparator_item = comparator[item_idx]
        comparator_group_values = [comparator[header.index(g)] for g in grouping_cols]

        candidate_referents = referent_map.get(item_label, [])
        if not candidate_referents:
            continue

        candidate_results = []
        for referent in candidate_referents:
            referent_speaker = referent[speaker_idx]
            referent_item = referent[item_idx]
            error_counts, alignment_steps = classify_alignment(
                comparator_item=comparator_item,
                referent_item=referent_item,
                comparator_speaker=comparator_speaker,
                item_label=item_label,
                referent_speaker=referent_speaker,
            )
            candidate_results.append((error_counts, alignment_steps, referent))

        chosen_error_counts, chosen_alignment_steps, chosen_referent = min(
            candidate_results,
            key=lambda x: x[0]["total_errors"],
        )

        chosen_referent_speaker = chosen_referent[speaker_idx]
        chosen_referent_item = chosen_referent[item_idx]

        # Item-level row
        row = (
            [comparator_speaker]
            + comparator_group_values
            + [
                item_label,
                chosen_referent_speaker,
                chosen_referent_item,
                comparator_item,
                chosen_error_counts["total_errors"],
                chosen_error_counts["vowel_for_consonant"],
                chosen_error_counts["consonant_for_vowel"],
                chosen_error_counts["insertion"],
                chosen_error_counts["deletion"],
                chosen_error_counts["vowel_sub"],
                chosen_error_counts["consonant_sub"],
            ]
        )

        if "unweighted" in algorithms:
            row.append(compute_unweighted_lev(
                chosen_error_counts["total_errors"],
                chosen_referent_item,
                comparator_item,
            ))
        if "weighted" in algorithms:
            row.append(compute_weighted_lev(
                chosen_error_counts,
                chosen_referent_item,
                comparator_item,
            ))
        if "PMI" in algorithms:
            row.append(np.nan)

        item_rows.append(row)

        # Slot-level rows
        if include_phoneme_alignments or "PMI" in algorithms or include_full_alignments:
            for step in chosen_alignment_steps:
                prow = (
                    [step["comparator_speaker"]]
                    + comparator_group_values
                    + [
                        step["item_label"],
                        step["referent_speaker"],
                        step["referent_item"],
                        step["comparator_item"],
                        step["referent_phoneme"],
                        step["comparator_phoneme"],
                        step["error_type"],
                        step["position"],
                        step["referent_length"],
                    ]
                )
                if "unweighted" in algorithms:
                    prow.append(get_unweighted_slot_penalty(step["error_type"]))
                if "weighted" in algorithms:
                    prow.append(get_weighted_slot_penalty(step["error_type"]))
                if "PMI" in algorithms:
                    prow.extend([np.nan, np.nan])
                phoneme_rows.append(prow)

    item_df = pd.DataFrame(item_rows[1:], columns=item_rows[0])
    phoneme_df = pd.DataFrame(phoneme_rows[1:], columns=phoneme_rows[0])

    # PMI second pass only if requested.
    if "PMI" in algorithms:
        phoneme_df, item_df = compute_pmi_second_pass(phoneme_df, item_df, grouping_cols)

    item_df, phoneme_df = restore_display_values(item_df, phoneme_df)
    item_df = round_numeric_columns(item_df, round_digits)
    phoneme_df = round_numeric_columns(phoneme_df, round_digits)

    full_alignments_df = None
    if include_full_alignments:
        full_alignments_df = build_full_alignments_rows(phoneme_df, item_df, grouping_cols, algorithms)

    if not include_phoneme_alignments:
        phoneme_return = None
    else:
        phoneme_return = phoneme_df

    metadata = {
        "n_rows": int(len(df_all)),
        "n_referents": int(len(referent_df)),
        "n_comparators": int(len(comparator_df)),
        "n_items": int(len(item_labels)),
        "n_groupings": int(len(grouping_cols)),
        "grouping_columns": grouping_cols,
        "grouping_labels": grouping_labels,
        "selected_algorithms": list(algorithms),
    }

    comparator_summary_df = build_comparator_speaker_summary(item_df, phoneme_df)

    return {
        "comparator_speaker_summary": comparator_summary_df,
        "item_error_counts": item_df,
        "phoneme_alignments": phoneme_return,
        "full_alignments": full_alignments_df,
        "grouping_labels": grouping_labels,
        "grouping_map": grouping_map,
        "metadata": metadata,
    }


def get_grouping_options(input_file: str) -> Dict[str, object]:
    """Lightweight helper for populating UI controls before running analysis."""
    df = _read_uploaded_table(input_file)
    df = df.dropna(how="all").fillna("")
    df.columns = [str(col).strip() for col in df.columns]

    grouping_cols = [col for col in df.columns if col.startswith("grouping_")]
    grouping_labels, grouping_map = get_grouping_options_from_columns(grouping_cols)

    grouping_levels: Dict[str, List[str]] = {}
    for label, col in grouping_map.items():
        if col in df.columns:
            values = (
                df[col]
                .astype(str)
                .str.strip()
                .replace("", np.nan)
                .dropna()
                .unique()
                .tolist()
            )
            grouping_levels[label] = sorted(values)
        else:
            grouping_levels[label] = []

    return {
        "grouping_columns": grouping_cols,
        "grouping_labels": grouping_labels,
        "grouping_map": grouping_map,
        "grouping_levels": grouping_levels,
    }




def _format_percent(value: float) -> str:
    """Format a proportion as a percentage string rounded to 2 decimals."""
    if pd.isna(value) or np.isinf(value):
        value = 0.0
    return f"{(float(value) * 100):.2f}%"


def build_comparator_speaker_summary(
    item_df: pd.DataFrame,
    phoneme_df: Optional[pd.DataFrame],
) -> pd.DataFrame:
    """
    Build the comparator_speaker_summary sheet.

    Columns:
        comparator_speaker
        errors_per_item
        percentage_phonemes_in_error
        total_error_count
        percentage_vowel_for_consonant
        percentage_consonant_for_vowel
        percentage_insertion
        percentage_deletion
        percentage_vowel_sub
        percentage_consonant_sub
    """
    columns = [
        "comparator_speaker",
        "errors_per_item",
        "percentage_phonemes_in_error",
        "total_error_count",
        "percentage_vowel_for_consonant",
        "percentage_consonant_for_vowel",
        "percentage_insertion",
        "percentage_deletion",
        "percentage_vowel_sub",
        "percentage_consonant_sub",
    ]

    if item_df is None or item_df.empty:
        return pd.DataFrame(columns=columns)

    item_summary = (
        item_df.groupby("comparator_speaker", dropna=False)
        .agg(
            errors_per_item=("total_errors", "mean"),
            total_error_count=("total_errors", "sum"),
        )
        .reset_index()
    )

    if phoneme_df is None or phoneme_df.empty:
        out = item_summary.copy()
        out["errors_per_item"] = out["errors_per_item"].round(2)
        out["total_error_count"] = out["total_error_count"].fillna(0).astype(int)
        out["percentage_phonemes_in_error"] = "0.00%"
        for col in columns[4:]:
            out[col] = "0.00%"
        return out[columns]

    phon_all = phoneme_df.copy()
    phon_all["error_type"] = phon_all["error_type"].astype(str)
    phon_nonalign = phon_all[phon_all["error_type"] != "alignment"].copy()

    total_slots = (
        phon_all.groupby("comparator_speaker", dropna=False)
        .size()
        .rename("total_slots")
        .reset_index()
    )
    error_slots = (
        phon_nonalign.groupby("comparator_speaker", dropna=False)
        .size()
        .rename("error_slots")
        .reset_index()
    )

    out = item_summary.merge(total_slots, on="comparator_speaker", how="left")
    out = out.merge(error_slots, on="comparator_speaker", how="left")
    out["total_slots"] = out["total_slots"].fillna(0)
    out["error_slots"] = out["error_slots"].fillna(0)

    out["percentage_phonemes_in_error"] = np.where(
        out["total_slots"] > 0,
        out["error_slots"] / out["total_slots"],
        0.0,
    )

    error_types = [
        "vowel_for_consonant",
        "consonant_for_vowel",
        "insertion",
        "deletion",
        "vowel_sub",
        "consonant_sub",
    ]

    type_counts = (
        phon_nonalign.groupby(["comparator_speaker", "error_type"], dropna=False)
        .size()
        .unstack(fill_value=0)
        .reset_index()
    )

    out = out.merge(type_counts, on="comparator_speaker", how="left")
    for et in error_types:
        if et not in out.columns:
            out[et] = 0
        out[et] = out[et].fillna(0)

    out["errors_per_item"] = out["errors_per_item"].round(2)
    out["total_error_count"] = out["total_error_count"].fillna(0).astype(int)

    for et in error_types:
        pct_col = f"percentage_{et}"
        out[pct_col] = np.where(
            out["total_error_count"] > 0,
            out[et] / out["total_error_count"],
            0.0,
        )

    out["percentage_phonemes_in_error"] = out["percentage_phonemes_in_error"].map(_format_percent)
    for et in error_types:
        out[f"percentage_{et}"] = out[f"percentage_{et}"].map(_format_percent)

    return out[columns]


def build_download_workbook(
    results: Dict[str, object],
    include_phoneme_alignments: bool = True,
    include_full_alignments: bool = False,
) -> bytes:
    """Return an Excel workbook as bytes for Shiny download handlers."""
    comparator_summary_df = results.get("comparator_speaker_summary")
    item_df = results["item_error_counts"]
    phoneme_df = results.get("phoneme_alignments")
    full_alignments_df = results.get("full_alignments")

    if comparator_summary_df is None:
        comparator_summary_df = build_comparator_speaker_summary(item_df, phoneme_df)

    buffer = BytesIO()
    with pd.ExcelWriter(buffer, engine="xlsxwriter") as writer:
        comparator_summary_df.to_excel(writer, index=False, sheet_name="comparator_speaker_summary")
        item_df.to_excel(writer, index=False, sheet_name="item_error_counts")

        if include_phoneme_alignments and phoneme_df is not None:
            phoneme_df.to_excel(writer, index=False, sheet_name="phoneme_alignments")

        if include_full_alignments and full_alignments_df is not None:
            full_alignments_df.to_excel(writer, index=False, header=False, sheet_name="full_alignments")

    buffer.seek(0)
    return buffer.getvalue()


def build_alignment_workbook(results: Dict[str, object]) -> bytes:
    """Return the LEAAP_alignment workbook as bytes for Shiny download handlers."""
    full_alignments_df = results.get("full_alignments")
    if full_alignments_df is None:
        raise LEAAPError(
            "Full alignments were not generated. Run analyze_file(..., include_full_alignments=True) first."
        )

    buffer = BytesIO()
    with pd.ExcelWriter(buffer, engine="xlsxwriter") as writer:
        full_alignments_df.to_excel(writer, index=False, header=False, sheet_name="full_alignments")

    buffer.seek(0)
    return buffer.getvalue()


# =========================================================
# INPUT LOADING / VALIDATION
# =========================================================


def load_input_data(filepath: str) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, List[str], List[str]]:
    df = _read_uploaded_table(filepath)
    df = df.dropna(how="all").fillna("")
    df.columns = [str(col).strip() for col in df.columns]

    required_cols = {"speaker", "referent_speakers"}
    missing_cols = required_cols - set(df.columns)
    if missing_cols:
        raise LEAAPError(f"Missing required columns: {sorted(missing_cols)}")

    grouping_cols = [col for col in df.columns if col.startswith("grouping_")]
    metadata_cols = ["speaker"] + grouping_cols + ["referent_speakers"]
    item_labels = [col for col in df.columns if col not in metadata_cols]
    if not item_labels:
        raise LEAAPError("No item columns were found in the uploaded file.")

    df["speaker"] = df["speaker"].astype(str).str.strip()
    df["referent_speakers"] = df["referent_speakers"].astype(str).str.strip().str.lower()
    df = df[df["speaker"] != ""]

    if df.empty:
        raise LEAAPError("No non-empty speaker rows were found in the uploaded file.")

    referent_df = df[df["referent_speakers"] == "referent"].copy()
    comparator_df = df[df["referent_speakers"] != "referent"].copy()

    if referent_df.empty:
        raise LEAAPError("At least 1 referent speaker is required.")
    if comparator_df.empty:
        raise LEAAPError("At least 1 comparator speaker is required.")

    return df, referent_df, comparator_df, grouping_cols, item_labels


# =========================================================
# GROUPING HELPERS
# =========================================================


def get_grouping_options_from_columns(grouping_cols: Sequence[str]) -> Tuple[List[str], Dict[str, str]]:
    grouping_labels = [col.replace("grouping_", "", 1) for col in grouping_cols]
    grouping_map = dict(zip(grouping_labels, grouping_cols))
    return grouping_labels, grouping_map


# =========================================================
# WIDE -> LONG HELPERS
# =========================================================


def reshape_to_long(df: pd.DataFrame, grouping_cols: Sequence[str], item_labels: Sequence[str]) -> List[List[object]]:
    long_rows: List[List[object]] = []
    header = ["speaker"] + list(grouping_cols) + ["item_label", "item", "referent_speakers"]
    long_rows.append(header)

    for _, row in df.iterrows():
        for item_label in item_labels:
            long_rows.append(
                [row["speaker"]]
                + [row[g] for g in grouping_cols]
                + [item_label, row[item_label], row["referent_speakers"]]
            )

    return long_rows


def split_long_by_role(long_data: List[List[object]]) -> Tuple[List[List[object]], List[List[object]]]:
    header = long_data[0]
    role_idx = header.index("referent_speakers")

    referent_long = [header]
    comparator_long = [header]

    for row in long_data[1:]:
        if str(row[role_idx]).strip().lower() == "referent":
            referent_long.append(row)
        else:
            comparator_long.append(row)

    if len(referent_long) <= 1:
        raise LEAAPError("No referent rows were found after reshaping.")
    if len(comparator_long) <= 1:
        raise LEAAPError("No comparator rows were found after reshaping.")

    return referent_long, comparator_long


def build_referent_map(referent_long: List[List[object]]) -> Dict[str, List[List[object]]]:
    header = referent_long[0]
    item_label_idx = header.index("item_label")

    referent_map: Dict[str, List[List[object]]] = {}
    for row in referent_long[1:]:
        referent_map.setdefault(row[item_label_idx], []).append(row)
    return referent_map


# =========================================================
# PREPROCESSING / RESTORATION
# =========================================================


def preprocess_word(word: object) -> str:
    word = str(word)
    word = word.replace('͡', '')
    for seq, placeholder in FORWARD_REPLACEMENTS.items():
        word = word.replace(seq, placeholder)
    return word


def restore_display_phoneme(text: object) -> object:
    if pd.isna(text):
        return text
    text = str(text)
    for placeholder, ipa_value in REVERSE_REPLACEMENTS.items():
        text = text.replace(placeholder, ipa_value)
    return text


def restore_display_values(item_df: pd.DataFrame, phoneme_df: pd.DataFrame) -> Tuple[pd.DataFrame, pd.DataFrame]:
    item_df = item_df.copy()
    phoneme_df = phoneme_df.copy()

    for col in ["referent_item", "comparator_item"]:
        if col in item_df.columns:
            item_df[col] = item_df[col].apply(restore_display_phoneme)

    for col in ["referent_item", "comparator_item", "referent_phoneme", "comparator_phoneme"]:
        if col in phoneme_df.columns:
            phoneme_df[col] = phoneme_df[col].apply(restore_display_phoneme)

    return item_df, phoneme_df


# =========================================================
# ALIGNMENT CORE
# =========================================================


def _add_severity(a: Tuple[int, int, int], b: Tuple[int, int, int]) -> Tuple[int, int, int]:
    return tuple(x + y for x, y in zip(a, b))


def _get_substitution_type(comparator_char: str, referent_char: str) -> str:
    if comparator_char in ipa_vowels and referent_char in ipa_consonants:
        return "vowel_for_consonant"
    if comparator_char in ipa_consonants and referent_char in ipa_vowels:
        return "consonant_for_vowel"
    if comparator_char in ipa_vowels and referent_char in ipa_vowels:
        return "vowel_sub"
    if comparator_char in ipa_consonants and referent_char in ipa_consonants:
        return "consonant_sub"
    return "unknown_substitution"


def _substitution_severity_delta(comparator_char: str, referent_char: str) -> Tuple[int, int, int]:
    sub_type = _get_substitution_type(comparator_char, referent_char)
    if sub_type in ("vowel_sub", "consonant_sub"):
        return (0, 0, 0)
    if sub_type in ("vowel_for_consonant", "consonant_for_vowel"):
        return (1, 0, 0)
    return (0, 0, 1)


def build_edit_and_severity_matrices(s1: str, s2: str) -> Tuple[np.ndarray, List[List[Tuple[int, int, int]]]]:
    m, n = len(s1), len(s2)
    dist = np.zeros((m + 1, n + 1), dtype=float)
    sev: List[List[Tuple[int, int, int]]] = [[None] * (n + 1) for _ in range(m + 1)]
    sev[0][0] = (0, 0, 0)

    for i in range(1, m + 1):
        dist[i][0] = i
        sev[i][0] = _add_severity(sev[i - 1][0], (0, 1, 0))

    for j in range(1, n + 1):
        dist[0][j] = j
        sev[0][j] = _add_severity(sev[0][j - 1], (0, 1, 0))

    for i in range(1, m + 1):
        for j in range(1, n + 1):
            candidates = []

            if s1[i - 1] == s2[j - 1]:
                candidates.append((dist[i - 1][j - 1], sev[i - 1][j - 1]))
            else:
                candidates.append((
                    dist[i - 1][j - 1] + 1,
                    _add_severity(sev[i - 1][j - 1], _substitution_severity_delta(s1[i - 1], s2[j - 1])),
                ))

            candidates.append((dist[i - 1][j] + 1, _add_severity(sev[i - 1][j], (0, 1, 0))))
            candidates.append((dist[i][j - 1] + 1, _add_severity(sev[i][j - 1], (0, 1, 0))))

            best_dist = min(c[0] for c in candidates)
            best_sev = min(c[1] for c in candidates if c[0] == best_dist)

            dist[i][j] = best_dist
            sev[i][j] = best_sev

    return dist, sev


def _standard_backtrace_map(s1: str, s2: str, dist: np.ndarray) -> Dict[Tuple[int, int], str]:
    i, j = len(s1), len(s2)
    standard_choice: Dict[Tuple[int, int], str] = {}

    while i > 0 or j > 0:
        if i > 0 and j > 0 and s1[i - 1] == s2[j - 1] and dist[i][j] == dist[i - 1][j - 1]:
            standard_choice[(i, j)] = "M"
            i -= 1
            j -= 1
        elif j > 0 and dist[i][j] == dist[i][j - 1] + 1:
            standard_choice[(i, j)] = "D"
            j -= 1
        elif i > 0 and dist[i][j] == dist[i - 1][j] + 1:
            standard_choice[(i, j)] = "I"
            i -= 1
        elif i > 0 and j > 0 and dist[i][j] == dist[i - 1][j - 1] + 1:
            standard_choice[(i, j)] = "S"
            i -= 1
            j -= 1
        else:
            raise LEAAPError(f"Could not trace alignment path at i={i}, j={j}.")

    return standard_choice


def classify_alignment(
    comparator_item: str,
    referent_item: str,
    comparator_speaker: str,
    item_label: str,
    referent_speaker: str,
) -> Tuple[Dict[str, int], List[Dict[str, object]]]:
    s1 = preprocess_word(comparator_item)
    s2 = preprocess_word(referent_item)

    dist, sev = build_edit_and_severity_matrices(s1, s2)
    standard_choice = _standard_backtrace_map(s1, s2, dist)

    i, j = len(s1), len(s2)
    referent_length = len(s2)
    errors = {
        "total_errors": int(dist[i][j]),
        "vowel_for_consonant": 0,
        "consonant_for_vowel": 0,
        "insertion": 0,
        "deletion": 0,
        "vowel_sub": 0,
        "consonant_sub": 0,
    }
    alignment_steps: List[Dict[str, object]] = []

    while i > 0 or j > 0:
        candidates: List[str] = []

        if (
            i > 0 and j > 0 and
            s1[i - 1] == s2[j - 1] and
            dist[i][j] == dist[i - 1][j - 1] and
            sev[i][j] == sev[i - 1][j - 1]
        ):
            candidates.append("M")

        if (
            j > 0 and
            dist[i][j] == dist[i][j - 1] + 1 and
            sev[i][j] == _add_severity(sev[i][j - 1], (0, 1, 0))
        ):
            candidates.append("D")

        if (
            i > 0 and
            dist[i][j] == dist[i - 1][j] + 1 and
            sev[i][j] == _add_severity(sev[i - 1][j], (0, 1, 0))
        ):
            candidates.append("I")

        if (
            i > 0 and j > 0 and
            s1[i - 1] != s2[j - 1] and
            dist[i][j] == dist[i - 1][j - 1] + 1 and
            sev[i][j] == _add_severity(sev[i - 1][j - 1], _substitution_severity_delta(s1[i - 1], s2[j - 1]))
        ):
            candidates.append("S")

        if not candidates:
            raise LEAAPError(f"No valid traceback move at i={i}, j={j}.")

        preferred = standard_choice.get((i, j))
        if preferred in candidates:
            move = preferred
        else:
            move = next(x for x in ["M", "D", "I", "S"] if x in candidates)

        if move == "M":
            alignment_steps.append({
                "comparator_speaker": comparator_speaker,
                "item_label": item_label,
                "referent_speaker": referent_speaker,
                "referent_item": referent_item,
                "comparator_item": comparator_item,
                "referent_phoneme": s2[j - 1],
                "comparator_phoneme": s1[i - 1],
                "error_type": "alignment",
                "position": j - 1,
                "referent_length": referent_length,
            })
            i -= 1
            j -= 1

        elif move == "D":
            errors["deletion"] += 1
            alignment_steps.append({
                "comparator_speaker": comparator_speaker,
                "item_label": item_label,
                "referent_speaker": referent_speaker,
                "referent_item": referent_item,
                "comparator_item": comparator_item,
                "referent_phoneme": s2[j - 1],
                "comparator_phoneme": "",
                "error_type": "deletion",
                "position": j - 1,
                "referent_length": referent_length,
            })
            j -= 1

        elif move == "I":
            errors["insertion"] += 1
            alignment_steps.append({
                "comparator_speaker": comparator_speaker,
                "item_label": item_label,
                "referent_speaker": referent_speaker,
                "referent_item": referent_item,
                "comparator_item": comparator_item,
                "referent_phoneme": "",
                "comparator_phoneme": s1[i - 1],
                "error_type": "insertion",
                "position": j,
                "referent_length": referent_length,
            })
            i -= 1

        elif move == "S":
            comparator_char = s1[i - 1]
            referent_char = s2[j - 1]
            sub_type = _get_substitution_type(comparator_char, referent_char)
            if sub_type == "vowel_for_consonant":
                errors["vowel_for_consonant"] += 1
            elif sub_type == "consonant_for_vowel":
                errors["consonant_for_vowel"] += 1
            elif sub_type == "vowel_sub":
                errors["vowel_sub"] += 1
            else:
                errors["consonant_sub"] += 1
                if sub_type == "unknown_substitution":
                    sub_type = "consonant_sub"

            alignment_steps.append({
                "comparator_speaker": comparator_speaker,
                "item_label": item_label,
                "referent_speaker": referent_speaker,
                "referent_item": referent_item,
                "comparator_item": comparator_item,
                "referent_phoneme": referent_char,
                "comparator_phoneme": comparator_char,
                "error_type": sub_type,
                "position": j - 1,
                "referent_length": referent_length,
            })
            i -= 1
            j -= 1

    alignment_steps.reverse()
    return errors, alignment_steps


# =========================================================
# PENALTIES / DISTANCES
# =========================================================


def get_unweighted_slot_penalty(error_type: str) -> float:
    return 0.0 if error_type == "alignment" else 1.0


def get_weighted_slot_penalty(error_type: str) -> float:
    if error_type == "alignment":
        return 0.0
    if error_type == "vowel_sub":
        return 0.5
    if error_type == "consonant_sub":
        return 0.75
    if error_type == "insertion":
        return 1.0
    if error_type in ("deletion", "vowel_for_consonant", "consonant_for_vowel"):
        return 0.4
    return 0.4


def compute_change_to_length_penalty(referent_item: str, comparator_item: str, has_indel: bool) -> float:
    if not has_indel:
        return 0.0
    referent_length = len(preprocess_word(referent_item))
    comparator_length = len(preprocess_word(comparator_item))
    max_len = max(referent_length, comparator_length)
    if max_len <= 1:
        return 0.0
    return 1 / math.log10(max_len)


def compute_unweighted_lev(total_errors: int, referent_item: str, comparator_item: str) -> float:
    referent_length = len(preprocess_word(referent_item))
    comparator_length = len(preprocess_word(comparator_item))
    denom = max(referent_length, comparator_length)
    return 0.0 if denom == 0 else total_errors / denom


def compute_weighted_lev(error_counts: Dict[str, int], referent_item: str, comparator_item: str) -> float:
    weighted_sum = 0.0
    weighted_sum += error_counts["vowel_sub"] * 0.5
    weighted_sum += error_counts["consonant_sub"] * 0.75
    weighted_sum += error_counts["insertion"] * 1.0
    weighted_sum += error_counts["deletion"] * 0.4
    weighted_sum += error_counts["vowel_for_consonant"] * 0.4
    weighted_sum += error_counts["consonant_for_vowel"] * 0.4

    has_indel = (error_counts["insertion"] > 0 or error_counts["deletion"] > 0)
    weighted_sum += compute_change_to_length_penalty(referent_item, comparator_item, has_indel)
    return weighted_sum


# =========================================================
# PMI SECOND PASS
# =========================================================


def compute_pmi_second_pass(
    phoneme_df: pd.DataFrame,
    item_df: pd.DataFrame,
    grouping_cols: Sequence[str],
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    if phoneme_df.empty:
        item_df = item_df.copy()
        phoneme_df = phoneme_df.copy()
        item_df["PMI_lev"] = np.nan
        phoneme_df["pmi_penalty"] = np.nan
        phoneme_df["pmi_value"] = np.nan
        return phoneme_df, item_df

    df = phoneme_df.copy()
    null_symbol = "∅"
    df["ref_sym"] = df["referent_phoneme"].replace("", null_symbol)
    df["comp_sym"] = df["comparator_phoneme"].replace("", null_symbol)

    total_slots = len(df)
    pair_counts = Counter(zip(df["ref_sym"], df["comp_sym"]))
    ref_counts = Counter(df["ref_sym"])
    comp_counts = Counter(df["comp_sym"])

    pmi_values = {}
    for pair, pair_count in pair_counts.items():
        ref_sym, comp_sym = pair
        pxy = pair_count / total_slots
        px = ref_counts[ref_sym] / total_slots
        py = comp_counts[comp_sym] / total_slots
        pmi_values[pair] = math.log2((pxy / (px * py)) + 1)

    non_identity_pmis = [
        value for (ref_sym, comp_sym), value in pmi_values.items()
        if ref_sym != comp_sym
    ]
    max_pmi = max(non_identity_pmis) if non_identity_pmis else 0.0
    min_pmi = min(non_identity_pmis) if non_identity_pmis else 0.0

    def pmi_to_distance(ref_sym: str, comp_sym: str) -> float:
        if ref_sym == comp_sym:
            return 0.0
        pmi_value = pmi_values[(ref_sym, comp_sym)]
        if max_pmi == min_pmi:
            return 1.0
        return (max_pmi - pmi_value) / (max_pmi - min_pmi)

    df["pmi_value"] = [pmi_values[(r, c)] for r, c in zip(df["ref_sym"], df["comp_sym"])]
    df["pmi_penalty"] = [pmi_to_distance(r, c) for r, c in zip(df["ref_sym"], df["comp_sym"])]

    group_cols = ["comparator_speaker"] + list(grouping_cols) + [
        "item_label", "referent_speaker", "referent_item", "comparator_item"
    ]

    item_pmi = (
        df.groupby(group_cols, dropna=False)
        .agg(pmi_penalty_sum=("pmi_penalty", "sum"), alignment_slots=("error_type", "count"))
        .reset_index()
    )
    item_pmi["PMI_lev"] = item_pmi.apply(
        lambda row: 0.0 if row["alignment_slots"] == 0 else row["pmi_penalty_sum"] / row["alignment_slots"],
        axis=1,
    )

    merge_cols = ["comparator_speaker"] + list(grouping_cols) + [
        "item_label", "referent_speaker", "referent_item", "comparator_item"
    ]
    item_df = item_df.drop(columns=["PMI_lev"], errors="ignore").merge(
        item_pmi[merge_cols + ["PMI_lev"]],
        on=merge_cols,
        how="left",
    )

    df = df.drop(columns=["ref_sym", "comp_sym"])
    return df, item_df


# =========================================================
# FULL ALIGNMENT DISPLAY
# =========================================================


def build_full_alignments_rows(
    phoneme_df: pd.DataFrame,
    item_df: pd.DataFrame,
    grouping_cols: Sequence[str],
    algorithms: Sequence[str],
) -> pd.DataFrame:
    if phoneme_df.empty:
        return pd.DataFrame()

    lookup_cols = ["comparator_speaker"] + list(grouping_cols) + [
        "item_label", "referent_speaker", "referent_item", "comparator_item"
    ]
    item_lookup = item_df.set_index(lookup_cols)

    display_rows: List[List[object]] = []
    group_cols = ["comparator_speaker"] + list(grouping_cols) + [
        "item_label", "referent_speaker", "referent_item", "comparator_item"
    ]

    grouped = phoneme_df.groupby(group_cols, dropna=False, sort=False)

    for key, block in grouped:
        row_meta = dict(zip(group_cols, key))
        lookup_key = (
            row_meta["comparator_speaker"],
            *[row_meta[g] for g in grouping_cols],
            row_meta["item_label"],
            row_meta["referent_speaker"],
            row_meta["referent_item"],
            row_meta["comparator_item"],
        )
        item_row = item_lookup.loc[lookup_key]
        if isinstance(item_row, pd.DataFrame):
            item_row = item_row.iloc[0]

        referent_phonemes = block["referent_phoneme"].tolist()
        comparator_phonemes = block["comparator_phoneme"].tolist()
        error_types = block["error_type"].tolist()

        display_rows.append([row_meta["referent_speaker"], row_meta["item_label"]] + referent_phonemes)
        display_rows.append([row_meta["comparator_speaker"], row_meta["item_label"]] + comparator_phonemes)
        display_rows.append(["", "error_type"] + error_types)

        if "unweighted" in algorithms:
            display_rows.append(
                ["", "unweighted_penalty"] + block["unweighted_penalty"].tolist() + ["Distance Value", float(item_row["unweighted_lev"])]
            )
        if "weighted" in algorithms:
            display_rows.append(
                ["", "weighted_penalty"] + block["weighted_penalty"].tolist() + ["Distance Value", float(item_row["weighted_lev"])]
            )
        if "PMI" in algorithms:
            display_rows.append(
                ["", "PMI_penalty"] + block["pmi_penalty"].fillna("").tolist() + ["Distance Value", float(item_row["PMI_lev"]) if pd.notna(item_row["PMI_lev"]) else ""]
            )

        display_rows.append([])
        display_rows.append([])

    max_len = max(len(row) for row in display_rows) if display_rows else 0
    padded_rows = [row + [""] * (max_len - len(row)) for row in display_rows]
    return pd.DataFrame(padded_rows)


# =========================================================
# UTILITY HELPERS
# =========================================================


def round_numeric_columns(df: pd.DataFrame, digits: int = 6) -> pd.DataFrame:
    df = df.copy()
    numeric_cols = df.select_dtypes(include=[np.number]).columns
    if len(numeric_cols) > 0:
        df[numeric_cols] = df[numeric_cols].round(digits)
    return df


def _normalize_algorithms(selected_algorithms: Sequence[str]) -> Tuple[str, ...]:
    cleaned = []
    for alg in selected_algorithms:
        if alg is None:
            continue
        value = str(alg).strip()
        if value.upper() == "PMI":
            value = "PMI"
        else:
            value = value.lower()
        cleaned.append(value)

    cleaned = list(dict.fromkeys(cleaned))
    invalid = [alg for alg in cleaned if alg not in ALGORITHMS]
    if invalid:
        raise LEAAPError(f"Unsupported algorithm selection: {invalid}. Choose from {list(ALGORITHMS)}.")
    if not cleaned:
        raise LEAAPError("At least one algorithm must be selected.")
    return tuple(cleaned)


def _read_uploaded_table(filepath: str) -> pd.DataFrame:
    lower = filepath.lower()
    try:
        if lower.endswith(".csv"):
            return pd.read_csv(filepath)
        if lower.endswith(".xlsx"):
            return pd.read_excel(filepath)
    except Exception as exc:
        raise LEAAPError(f"Could not read uploaded file: {exc}") from exc
    raise LEAAPError("Uploaded file must be a .csv or .xlsx file.")
