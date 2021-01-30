import os
import sys

timeout = 600  # 10min timeout

input_files = [
    ["tailopt/sum.pmrs", ""],
    ["tailopt/mts.pmrs", "--verification=5"],
    ["tailopt/mps.pmrs", "--verification=5"],
    ["combine/mts.pmrs", "--verification=5"],
    ["combine/mts_and_mps.pmrs", "--verification=5"],
    ["zippers/sum.pmrs", ""],
    ["zippers/height.pmrs", ""],
    ["zippers/mips.pmrs", ""],
    ["zippers/leftmostodd.pmrs", "-b 4"],
    ["zippers/maxPathWeight.pmrs", ""],
    ["zippers/maxPathWeight2.pmrs", ""],
    ["ptree/sum.pmrs", ""],
    ["ptree/mul.pmrs", ""],
    ["ptree/maxheads.pmrs", ""],
    ["ptree/maxlast.pmrs", ""],
    ["ptree/maxsum.pmrs", ""],
    ["tree/sumtree.pmrs", ""],
    ["tree/maxtree.pmrs", ""],
    ["tree/min.pmrs", ""],
    ["tree/minmax.pmrs", ""],
    ["tree/maxtree2.pmrs", ""],
    ["tree/poly.pmrs", ""],
    ["tree/maxPathWeight.pmrs", ""],
    ["list/sumhom.pmrs", ""],
    ["list/sumodds.pmrs", ""],
    ["list/sumgt.pmrs", ""],
    ["list/sumevens.pmrs", ""],
    ["list/lenhom.pmrs", ""],
    ["list/prodhom.pmrs", ""],
    ["list/polyhom.pmrs", ""],
    ["list/hamming.pmrs", ""],
    ["list/maxhom.pmrs", ""],
    ["list/maxcount.pmrs", ""],
    ["list/minhom.pmrs", ""],
    ["list/mincount.pmrs", ""],
    ["list/last.pmrs", ""],
    ["list/sndminhom.pmrs", ""],
    ["list/mtshom.pmrs", ""],
    ["list/mpshom.pmrs", ""],
    ["list/msshom.pmrs", ""],
    ["list/search.pmrs", ""],
    ["list/line_of_sight.pmrs", ""],
    ["list/mts_and_mps_hom.pmrs", ""],
    ["list/issorted.pmrs", "-t"],
    ["tree/sorted.pmrs", "-t"],
    ["tree/mips.pmrs", ""],
    ["tree/mits.pmrs", ""],
    ["tree/mpps.pmrs", ""]
]

root = os.getcwd()
path = os.path.join(root, "_build/default/bin/Atropos.exe")

sys.stdout.flush()

for filename_with_opt in input_files:
    filename = filename_with_opt[0]
    opt = filename_with_opt[1]
    print("B:%s,requation" % filename)
    sys.stdout.flush()
    os.system("timeout %i %s %s -i %s" %
              (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt))
    if len(sys.argv) >= 2:
        do_unoptimized = True
        if int(sys.argv[1]) > 0:
            do_unoptimized = False
        # Detupling off
        # print("B:%s,requation+t" % filename)
        # sys.stdout.flush()
        # os.system("timeout %i %s %s -it %s" %
        #           (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt))
        if do_unoptimized:
            # Detupling + splitting off
            print("B:%s,requation+st" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -ist %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt))
            # Syntactic definitions off
            print("B:%s,requation+d" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -i --no-syndef %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt))
            # All optimizations off
            print("B:%s,requation+off" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -ist --no-syndef %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt))
        # =========================================================================================
        # ===> ACEGIS algorithm evaluation.
        naive_opt = "--use-acegis"
        print("B:%s,naive" % filename)
        sys.stdout.flush()
        os.system("timeout %i %s %s -i %s %s" %
                  (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
        if do_unoptimized:
            # # Detupling off
            # print("B:%s,naive+t" % filename)
            # sys.stdout.flush()
            # os.system("timeout %i %s %s -it %s %s" %
            #           (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
            # Detupling + splitting off
            print("B:%s,naive+st" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -ist %s %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
            # Syntactic definitions off
            print("B:%s,naive+d" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -i --no-syndef %s %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
            print("B:%s,naive+off" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -ist --no-syndef %s %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))

         # ===> CCEGIS algorithm evaluation.
        naive_opt = "--use-ccegis"
        print("B:%s,ccegis" % filename)
        sys.stdout.flush()
        os.system("timeout %i %s %s -i %s %s" %
                  (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
        if do_unoptimized:
            # # Detupling off
            # print("B:%s,naive+t" % filename)
            # sys.stdout.flush()
            # os.system("timeout %i %s %s -it %s %s" %
            #           (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
            # Detupling + splitting off
            print("B:%s,ccegis+st" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -ist %s %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
            # Syntactic definitions off
            print("B:%s,ccegis+d" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -i --no-syndef %s %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
            print("B:%s,ccegis+off" % filename)
            sys.stdout.flush()
            os.system("timeout %i %s %s -ist --no-syndef %s %s" %
                      (timeout, path, os.path.realpath(os.path.join("benchmarks", filename)), opt, naive_opt))
