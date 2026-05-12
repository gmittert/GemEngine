use bullet_lib::{
    game::inputs::Chess768,
    nn::optimiser::AdamWOptimiser,
    trainer::save::SavedFormat,
    value::{NoOutputBuckets, ValueTrainer, ValueTrainerBuilder},
};

pub fn get_trainer() -> ValueTrainer<AdamWOptimiser, Chess768, NoOutputBuckets> {
    ValueTrainerBuilder::default()
        .dual_perspective()
        .optimiser(bullet_lib::nn::optimiser::AdamW)
        .inputs(Chess768)
        .save_format(&[
            SavedFormat::id("l0w").quantise::<i16>(nnue::QA),
            SavedFormat::id("l0b").quantise::<i16>(nnue::QA),
            SavedFormat::id("l1w").quantise::<i16>(nnue::QB),
            SavedFormat::id("l1b").quantise::<i16>(nnue::QB),
        ])
        .loss_fn(|output, target| output.sigmoid().squared_error(target))
        .build(|builder, stm_inputs, ntm_inputs| {
            // weights
            let l0 = builder.new_affine("l0", 768, nnue::HIDDEN_SIZE);
            let l1 = builder.new_affine("l1", 2 * nnue::HIDDEN_SIZE, 1);

            // inference
            let stm_hidden = l0.forward(stm_inputs).screlu();
            let ntm_hidden = l0.forward(ntm_inputs).screlu();
            let hidden_layer = stm_hidden.concat(ntm_hidden);
            l1.forward(hidden_layer)
        })
}
