The artifact for the paper _Feature Trace Recording_ is a software library written in the Haskell language.
The library is accompanied with a demo application that uses the library to reproduce our motivating example (Alice and Bob using feature trace recording in Section 2 in our paper) as well as examples of the edit patterns we used to evaluate feature trace recording (Section 5).
Our artefact has the following purposes:
- **Reusability**: Other researchers can integrate the library into future benchmarks and studies to compare their future methods against feature trace recording.
Moreover, researchers can integrate our library into future prototypes that build upon our results.
Practitioners can reuse the library to tether feature trace recording to dedicated target systems, including version control systems or IDEs (see Sections 2.1 and 4.1).
To this end, feature trace recording as well as the library are designed as an extensible framework to even allow integrating custom recording functions for specific tasks (Section 4.2).
We documented all modules of the library in detail and made the documentation [available on Github][documentation] as well.
- **Formal Specification and Machine-Checked Proofs**: Our library shows that the formal model of feature trace recording presented in our paper is suited to be implemented directly.
By using a purely functional programming language, we show that the formal model of feature trace recording presented in the paper (Section 5) can be turned into runnable code almost one-to-one*.
To this end, the library itself serves as an extended formal model, including data types for all parts of the toolchain such as abstract syntax trees and edits.
Implementing feature trace recording in a pure functional way, also allows us to employ machine-checked proofs in the future.
- **Giving Examples for Evaluation**: Our demo complements our evaluation with concrete examples for the inspected edit patterns (Section 5.2) to show that our library is able to reproduce the results of our evaluation.
Therefore, our demo runs feature trace recording on an instance of each pattern (i.e., a concrete edit matching this pattern).

We claim the _Artifacts Available_ badge as we made our artefacts publicly available on [Github][ftrgithub] and [Zenodo][ftrzenodo]. (For now, the Zenodo link points to a reserved doi under which we will publish the artifacts once completed until the deadline at June 4.)

We claim the _Artifacts Evaluated Reusable_ badge as we implemented feature trace recording as a reusable library (see above).
Furthermore, our library serves as a reference implementation if researchers or practitioners want to reimplement feature trace recording in another programming language.

*: One may want to compare the formal model presented in Section 4.2 with [src/feature/recording/DefaultFeatureTraceRecording.hs](src/feature/recording/DefaultFeatureTraceRecording.hs).

[ftrgithub]: https://github.com/pmbittner/FeatureTraceRecording/tree/esecfse21
[ftrzenodo]: https://doi.org/10.5281/zenodo.4818461
[documentation]: https://pmbittner.github.io/FeatureTraceRecording/