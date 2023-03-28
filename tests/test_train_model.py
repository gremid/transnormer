import copy
import os
import pytest
import tomli

import transformers
import torch

from transnormer.models import train_model
from transnormer.data import loader


def test_tokenize_input_and_output():
    # Load data
    path = "./tests/testdata/dtaeval/txt/arnima_invalide_1818-head10.txt"
    dataset = loader.load_dtaeval_as_dataset(path)

    # Load tokenizers
    tokenizer_input = transformers.AutoTokenizer.from_pretrained(
        "dbmdz/bert-base-historic-multilingual-cased"
    )
    tokenizer_labels = transformers.AutoTokenizer.from_pretrained(
        "bert-base-multilingual-cased"
    )

    # Parameters for tokenizing input and labels
    fn_kwargs = {
        "tokenizer_input": tokenizer_input,
        "tokenizer_output": tokenizer_labels,
        "max_length_input": 128,
        "max_length_output": 128,
    }

    # Do tokenization via mapping
    prepared_dataset = dataset.map(
        train_model.tokenize_input_and_output,
        fn_kwargs=fn_kwargs,
        batched=True,
        batch_size=1,
        remove_columns=["orig", "norm"],
    )

    assert len(prepared_dataset[0]["input_ids"]) == 128
    assert len(prepared_dataset[0]["attention_mask"]) == 128
    assert len(prepared_dataset[0]["labels"]) == 128


def test_config_file_structure():
    target_param_dict = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "data/interim/dtaeval/dtaeval-train.jsonl",
                "data/interim/deu_news_2020/deu_news_2020-train.jsonl",
            ],
            "paths_validation": [
                "data/interim/dtaeval/dtaeval-validation.jsonl",
                "data/interim/deu_news_2020/deu_news_2020-validation.jsonl",
            ],
            "paths_test": [
                "data/interim/dtaeval/dtaeval-test.jsonl",
                "data/interim/deu_news_2020/deu_news_2020-test.jsonl",
            ],
            "n_examples_train": [
                1_000_000_000,
                50_000,
            ],
            "n_examples_validation": [
                1_000_000_000,
                5_000,
            ],
            "n_examples_test": [
                1_000_000_000,
                5_000,
            ],
        },
        # "subset_sizes": {"train": 100, "validation": 10, "test": 1},
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "dbmdz/bert-base-historic-multilingual-cased",
            "checkpoint_decoder": "bert-base-multilingual-cased",
        },
        "training_hyperparams": {
            "batch_size": 10,
            "epochs": 10,
            "eval_steps": 1000,
            "eval_strategy": "steps",
            "save_steps": 10,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }

    path = "training_config.toml"
    with open(path, mode="rb") as fp:
        CONFIGS = tomli.load(fp)

    # assert that all keys are identical
    # recurse into nested dicts, to assert that inner keys are identical as well
    def all_keys_match(dict1, dict2):
        if set(dict1.keys()) == set(dict2.keys()):
            for key in dict1:
                if isinstance(dict1[key], dict):
                    if not all_keys_match(dict1[key], dict2[key]):
                        return False
            return True
        else:
            return False

    assert all_keys_match(CONFIGS, target_param_dict)


def test_load_and_merge_datasets_full_sets():
    CONFIGS = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_validation": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_test": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "n_examples_train": [
                1_000_000,
                1_000_000,
            ],
            "n_examples_validation": [
                1_000_000,
                1_000_000,
            ],
            "n_examples_test": [
                1_000_000,
                1_000_000,
            ],
        },
        # The following configs don't matter ...
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "prajjwal1/bert-tiny",
            "checkpoint_decoder": "prajjwal1/bert-tiny",
        },
        "training_hyperparams": {
            "batch_size": 10,
            "epochs": 10,
            "eval_steps": 1000,
            "eval_strategy": "steps",
            "save_steps": 10,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }

    dataset = train_model.load_and_merge_datasets(CONFIGS)
    assert dataset["train"].num_rows == 6
    assert dataset["validation"].num_rows == 6
    assert dataset["test"].num_rows == 6


def test_load_and_merge_datasets_subsets1():
    CONFIGS = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_validation": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_test": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "n_examples_train": [
                3,
                3,
            ],
            "n_examples_validation": [
                2,
                2,
            ],
            "n_examples_test": [
                1,
                1,
            ],
        },
        # The following configs don't matter ...
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "prajjwal1/bert-tiny",
            "checkpoint_decoder": "prajjwal1/bert-tiny",
        },
        "training_hyperparams": {
            "batch_size": 10,
            "epochs": 10,
            "eval_steps": 1000,
            "eval_strategy": "steps",
            "save_steps": 10,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }

    dataset = train_model.load_and_merge_datasets(CONFIGS)
    assert dataset["train"].num_rows == 6
    assert dataset["validation"].num_rows == 4
    assert dataset["test"].num_rows == 2


def test_load_and_merge_datasets_subsets2():
    CONFIGS = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_validation": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_test": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "n_examples_train": [
                1_000_000,
                1_000_000,
            ],
            "n_examples_validation": [
                1_000_000,
                1_000_000,
            ],
            "n_examples_test": [
                1_000_000,
                1_000_000,
            ],
        },
        "subset_sizes": {"train": 3, "validation": 2, "test": 1},
        # The following configs don't matter ...
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "prajjwal1/bert-tiny",
            "checkpoint_decoder": "prajjwal1/bert-tiny",
        },
        "training_hyperparams": {
            "batch_size": 10,
            "epochs": 10,
            "eval_steps": 1000,
            "eval_strategy": "steps",
            "save_steps": 10,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }

    dataset = train_model.load_and_merge_datasets(CONFIGS)
    assert dataset["train"].num_rows == 3
    assert dataset["validation"].num_rows == 2
    assert dataset["test"].num_rows == 1


def test_tokenize_datasets():
    CONFIGS = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_validation": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_test": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "n_examples_train": [
                1_000_000,
                1_000_000,
            ],
            "n_examples_validation": [
                1_000_000,
                1_000_000,
            ],
            "n_examples_test": [
                1_000_000,
                1_000_000,
            ],
        },
        "subset_sizes": {"train": 3, "validation": 2, "test": 1},
        # The following configs don't matter ...
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "prajjwal1/bert-tiny",
            "checkpoint_decoder": "prajjwal1/bert-tiny",
        },
        "training_hyperparams": {
            "batch_size": 10,
            "epochs": 10,
            "eval_steps": 1000,
            "eval_strategy": "steps",
            "save_steps": 10,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }

    dataset = train_model.load_and_merge_datasets(CONFIGS)
    prepared_dataset, tok_in, tok_out = train_model.tokenize_datasets(dataset, CONFIGS)
    # Check if input_ids have desired type (torch.Tensor) and length (tokenizer.max_length_input)
    target_length = CONFIGS["tokenizer"]["max_length_input"]
    assert all(
        isinstance(prepared_dataset["train"]["input_ids"][i], torch.Tensor)
        for i in range(prepared_dataset["train"].num_rows)
    )
    assert all(
        len(prepared_dataset["train"]["input_ids"][i]) == target_length
        for i in range(prepared_dataset["train"].num_rows)
    )
    assert isinstance(tok_in, transformers.PreTrainedTokenizerBase)
    assert isinstance(tok_out, transformers.PreTrainedTokenizerBase)


def test_warmstart_seq2seq_model_normal():
    CONFIGS = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_validation": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_test": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "n_examples_train": [
                3,
                3,
            ],
            "n_examples_validation": [
                2,
                2,
            ],
            "n_examples_test": [
                1,
                1,
            ],
        },
        # "subset_sizes": {"train": 3, "validation": 2, "test": 1},
        # The following configs don't matter ...
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "prajjwal1/bert-tiny",
            "checkpoint_decoder": "prajjwal1/bert-tiny",
        },
        "training_hyperparams": {
            "batch_size": 10,
            "epochs": 10,
            "eval_steps": 1000,
            "eval_strategy": "steps",
            "save_steps": 10,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }
    device = torch.device(CONFIGS["gpu"] if torch.cuda.is_available() else "cpu")
    dataset = train_model.load_and_merge_datasets(CONFIGS)
    prepared_dataset, tok_in, tok_out = train_model.tokenize_datasets(dataset, CONFIGS)
    model = train_model.warmstart_seq2seq_model(CONFIGS, tok_out, device)
    # Check class
    assert isinstance(model, transformers.EncoderDecoderModel)
    # Check some configs
    assert model.config.num_beams == 4
    assert model.config.max_length == 128


@pytest.mark.skipif(not torch.cuda.is_available(), reason="training requires cuda")
def test_train_seq2seq_model():
    CONFIGS = {
        "gpu": "cuda:0",
        "random_seed": 42,
        "data": {
            "paths_train": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_validation": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "paths_test": [
                "tests/testdata/jsonl/dtaeval-train-head3.jsonl",
                "tests/testdata/jsonl/dtak-1600-1699-train-head3.jsonl",
            ],
            "n_examples_train": [
                3,
                3,
            ],
            "n_examples_validation": [
                2,
                2,
            ],
            "n_examples_test": [
                1,
                1,
            ],
        },
        # The following configs don't matter ...
        "tokenizer": {
            "max_length_input": 128,
            "max_length_output": 128,
            "input_transliterator": "Transliterator1",
        },
        "language_models": {
            "checkpoint_encoder": "prajjwal1/bert-tiny",
            "checkpoint_decoder": "prajjwal1/bert-tiny",
        },
        "training_hyperparams": {
            "batch_size": 1,
            "epochs": 1,
            "eval_steps": 1,
            "eval_strategy": "steps",
            "save_steps": 1,
            "fp16": True,
        },
        "beam_search_decoding": {
            "no_repeat_ngram_size": 3,
            "early_stopping": True,
            "length_penalty": 2.0,
            "num_beams": 4,
        },
    }
    device = torch.device(CONFIGS["gpu"] if torch.cuda.is_available() else "cpu")
    dataset = train_model.load_and_merge_datasets(CONFIGS)
    prepared_dataset, tok_in, tok_out = train_model.tokenize_datasets(dataset, CONFIGS)
    model = train_model.warmstart_seq2seq_model(CONFIGS, tok_out, device)

    model_untrained = copy.deepcopy(model)
    output_dir = "tests/testdata/tmp"
    # Training
    train_model.train_seq2seq_model(model, prepared_dataset, CONFIGS, output_dir)
    # Compare all states and check that some of them changed
    unequal_states = []
    for (name_mo, params_mo), (name_mn, params_mn) in zip(
        model_untrained.state_dict().items(), model.state_dict().items()
    ):
        assert name_mo == name_mn
        if not torch.equal(params_mo, params_mn):
            unequal_states.append(name_mo)
    assert len(unequal_states) > 0
    # Remove files that were created during training
    for root, dirs, files in os.walk(output_dir, topdown=False):
        for file in files:
            os.remove(os.path.join(root, file))
        else:
            os.rmdir(root)
