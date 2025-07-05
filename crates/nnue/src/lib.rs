use itertools::izip;
// Hyper parameters
pub const SUPERBATCHES: usize = 160;
pub const WDL_PROPORTION: f32 = 0.75;
pub const HIDDEN_SIZE: usize = 256;
pub const QA: i16 = 255;
pub const QB: i16 = 64;
pub const SCALE: i32 = 400;

pub static NNUE: Network = unsafe {
    std::mem::transmute(*include_bytes!(
        "../../../checkpoints/1_simple-50/quantised.bin"
    ))
};

/// Accumulator code cribbed from the bullet examples in
/// https://github.com/jw1912/bullet/blob/main/examples/simple.rs
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C, align(64))]
pub struct Accumulator {
    vals: [i16; HIDDEN_SIZE],
}

impl Accumulator {
    /// We initialize with just the baises. We don't have any pieces yet, we'll add their feature
    /// vectors as we add to the accumulator.
    pub fn new(net: &Network) -> Self {
        net.feature_biases
    }

    /// Add a feature to an accumulator.
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i += *d
        }
    }

    /// Simultaneously add and remove a feature to an accumulator.
    pub fn add_remove_feature(
        &mut self,
        add_feature_idx: usize,
        remove_feature_idx: usize,
        net: &Network,
    ) {
        for (acc, add, remove) in izip!(
            self.vals.iter_mut(),
            &net.feature_weights[add_feature_idx].vals,
            &net.feature_weights[remove_feature_idx].vals,
        ) {
            *acc += *add - *remove
        }
    }

    /// Simultaneously add two and remove one features to an accumulator.
    pub fn add2_remove1_feature(
        &mut self,
        add_feature1_idx: usize,
        add_feature2_idx: usize,
        remove_feature_idx: usize,
        net: &Network,
    ) {
        for (acc, add1, add2, remove) in izip!(
            self.vals.iter_mut(),
            &net.feature_weights[add_feature1_idx].vals,
            &net.feature_weights[add_feature2_idx].vals,
            &net.feature_weights[remove_feature_idx].vals,
        ) {
            *acc += *add1 + *add2 - *remove
        }
    }

    /// Simultaneously add one and remove two features to an accumulator.
    pub fn add1_remove2_feature(
        &mut self,
        add_feature_idx: usize,
        remove_feature_idx: usize,
        remove_feature2_idx: usize,
        net: &Network,
    ) {
        for (acc, add1, remove1, remove2) in izip!(
            self.vals.iter_mut(),
            &net.feature_weights[add_feature_idx].vals,
            &net.feature_weights[remove_feature_idx].vals,
            &net.feature_weights[remove_feature2_idx].vals,
        ) {
            *acc += *add1 - *remove1 - *remove2
        }
    }

    /// Simultaneously add and remove two features from an accumulator.
    pub fn add2_remove2_feature(
        &mut self,
        add_feature_idx: usize,
        add_feature2_idx: usize,
        remove_feature_idx: usize,
        remove_feature2_idx: usize,
        net: &Network,
    ) {
        for (acc, add1, add2, remove1, remove2) in izip!(
            self.vals.iter_mut(),
            &net.feature_weights[add_feature_idx].vals,
            &net.feature_weights[add_feature2_idx].vals,
            &net.feature_weights[remove_feature_idx].vals,
            &net.feature_weights[remove_feature2_idx].vals,
        ) {
            *acc += *add1 + *add2 - *remove1 - *remove2
        }
    }

    /// Remove a feature from an accumulator.
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i -= *d
        }
    }
}

/// A bullet quantized network
#[repr(C)]
pub struct Network {
    /// Column-Major `HIDDEN_SIZE x 768` matrix.
    feature_weights: [Accumulator; 768],
    /// Vector with dimension `HIDDEN_SIZE`.
    feature_biases: Accumulator,
    /// Column-Major `1 x (2 * HIDDEN_SIZE)` matrix, we use it like this to make the code nicer in
    /// `Network::evaluate`.
    output_weights: [i16; 2 * HIDDEN_SIZE],
    /// Scalar output bias.
    output_bias: i16,
}

#[inline]
/// Clipped ReLU - Activation Function.
/// Note that this takes the i16s in the accumulator to i32s.
fn crelu(x: i16) -> i32 {
    i32::from(x).clamp(0, i32::from(QA))
}

impl Network {
    /// Calculates the output of the network, starting from the already calculated hidden layer.
    pub fn evaluate(&self, us: &Accumulator, them: &Accumulator) -> i32 {
        // Initialise output with bias.
        let mut output = i32::from(self.output_bias);

        // Side-To-Move Accumulator -> Output.
        for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HIDDEN_SIZE]) {
            output += crelu(input) * i32::from(weight);
        }

        // Not-Side-To-Move Accumulator -> Output.
        for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HIDDEN_SIZE..]) {
            output += crelu(input) * i32::from(weight);
        }

        // Apply eval scale.
        output *= SCALE;

        // Remove quantisation.
        output /= i32::from(QA) * i32::from(QB);

        output
    }
}
