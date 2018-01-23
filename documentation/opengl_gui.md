# OpenGL GUI

The concept behind the OpenGL GUI is to investigate the effectiveness
of data visualization and interaction when using the capabilities of
modern graphics pipelines, where the data that is visualized is
produced in separate processes in an interactive manner.

The upshot is that the OpenGL GUI is not solely about presenting data
with 'photorealistic' graphics, but it is about interacting with data
to provide information through causal perception rather than
thoughtful analysis on the viewer's part.

This is a contrast to scientific visualization where data is
visualized in a manner for an expert to comprehend and see detail.

An example of this contrast would be the difference between viewing a
topographic geological map, and participating in the Disney Soarin'
ride; the latter has much less detail in the information conveyed, but
everybody who participates perceives the topography and geology of the
landscape.

The drivers behind ths GUI are manifold:

1. The prevalence of low cost, very high performance GPU platforms

2. The maturing of GPU pipelines with OpenGL 4 and Vulkan

3. The normalization of expectation of "photorealistic" rendering in
   modern games, where PC platforms now provided the highest quality
   rendering.

4. The improvement in human-computer interaction devices used in the
   gaming world, exemplified by the Nintendo Switch JoyCons.

5. The increase in the size of data sets, and the amount of data that
   can be accessed for visualiztion

6. The increase in heterogeneous processing, whereby datasets may be
   processed with higher efficiency on a desktop than ever before - be
   it through SIMD processing (GPUs), highly parallel CPU processing,
   or more specific processing (such as the memory-centric processing
   in the NFP platforms coming from the networking world), or highly
   bespoke processing through FPGAs tightly coupled to CPUs.

7. The increase in the number of processor cores readily available for
   processing without specific accelerators.

