use bullet_lib::{
    ExecutionContext,
    core::optimiser,
    default::Trainer,
    game::inputs::Chess768,
    nn::optimiser::AdamW,
    trainer::save::SavedFormat,
    value::{NoOutputBuckets, ValueTrainerBuilder},
};
// Hyper parameters
pub const SUPERBATCHES: usize = 160;
pub const WDL_PROPORTION: f32 = 0.75;
pub const HIDDEN_SIZE: usize = 256;
pub const QA: i16 = 255;
pub const QB: i16 = 64;
pub const SCALE: i32 = 400;

pub fn get_trainer() -> Trainer<optimiser::adam::AdamW<ExecutionContext>, Chess768, NoOutputBuckets>
{
    ValueTrainerBuilder::default()
        .dual_perspective()
        .optimiser(AdamW)
        .inputs(Chess768)
        .save_format(&[
            SavedFormat::id("l0w").quantise::<i16>(QA),
            SavedFormat::id("l0b").quantise::<i16>(QA),
            SavedFormat::id("l1w").quantise::<i16>(QB),
            SavedFormat::id("l1b").quantise::<i16>(QB),
        ])
        .loss_fn(|output, target| output.sigmoid().squared_error(target))
        .build(|builder, stm_inputs, ntm_inputs| {
            // weights
            let l0 = builder.new_affine("l0", 768, HIDDEN_SIZE);
            let l1 = builder.new_affine("l1", 2 * HIDDEN_SIZE, 1);

            // inference
            let stm_hidden = l0.forward(stm_inputs).screlu();
            let ntm_hidden = l0.forward(ntm_inputs).screlu();
            let hidden_layer = stm_hidden.concat(ntm_hidden);
            l1.forward(hidden_layer)
        })
}
