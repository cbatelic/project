namespace SimulinkClone.Core.Simulation

module Blocks =

    let constant (k: float) : StatelessBlock<unit,float> =
        fun () -> k

    let add : StatelessBlock<float*float, float> =
        fun (a,b) -> a + b

    let integrator (x0: float) : StatefulBlock<float,float,float> =
        { init = x0
          step = fun dt x u ->
              let y = x
              let xNext = x + dt * u
              y, xNext }

module Engine =

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

    let demoIntegratorRamp (dt: Dt) (steps: int) =
        let c1 = Blocks.constant 1.0
        let c2 = Blocks.constant 2.0
        let sum = Blocks.add
        let integ = Blocks.integrator 0.0

        let u (t: Time) =
            sum (c1 (), c2 ())

        runStateful dt steps u integ