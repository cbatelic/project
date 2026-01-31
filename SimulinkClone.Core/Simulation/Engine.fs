namespace SimulinkClone.Core.Simulation

module Blocks =

    let constant (k: float) : StatelessBlock<unit,float> =
        fun () -> k

    let add : StatelessBlock<float*float, float> =
        fun (a,b) -> a + b

    /// Diskretni integrator (Euler):
    /// x_{k+1} = x_k + dt * u_k
    /// y_k = x_k
    let integrator (x0: float) : StatefulBlock<float,float,float> =
        { init = x0
          step = fun dt x u ->
              let y = x
              let xNext = x + dt * u
              y, xNext }

module Engine =

    /// Pokreni stateful blok kroz N koraka uz input funkciju u(t)
    let runStateful
        (dt: Dt)
        (steps: int)
        (u: Time -> 'In)
        (block: StatefulBlock<'In,'Out,'State>)
        : Sample<'Out> list =
        
        let rec loop k t state acc =
            if k >= steps then
                List.rev acc
            else
                let input = u t
                let (y, state2) = block.step dt state input
                let sample = { t = t; value = y }
                loop (k + 1) (t + dt) state2 (sample :: acc)

        loop 0 0.0 block.init []

    /// Pokreni “ručno spojeni” mali pipeline:
    /// u(t) = add( constant(1), constant(2) ) = 3
    /// y(t) = integrator(0) of u(t)  => ramp 0, 0, 0.3, 0.6 ...
    let demoIntegratorRamp (dt: Dt) (steps: int) =
        let c1 = Blocks.constant 1.0
        let c2 = Blocks.constant 2.0
        let sum = Blocks.add
        let integ = Blocks.integrator 0.0

        let u (t: Time) =
            // u ne ovisi o t u ovom demo-u
            sum (c1 (), c2 ())

        runStateful dt steps u integ