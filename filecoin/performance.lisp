(in-package :filecoin)
(in-suite filecoin-suite)

#|
From Übercalc Compoments document:

Performance: for every $10 spent (at scale), it must be possible to seal 1GiB/hour.

Notes on how this is derived:

We assume that miners will not participate in the network unless doing so
is competitive with other potential uses of capital.
Compare with pure storage service business.
Upper bound on storage pricing is AWS Glacier.
S3 = ~$23/TiB-month = $276 / TiB-year [Tune this variable down to something more realistic.]
$50k / $276 = ~181TiB
Miner should be at capacity in ‘3 months’.
So must seal 181TiB/3months = 60TiB/month = ~2TiB/day = ~85GiB/hr
Pick some FGR interval: say 6 months.
Assume $10 / GiB/hr sealing: then sealing 85GiB/hr costs $850 up-front.
18 10TiB drives at $300 = $5400 up-front.
Should amortize costs over 6 months. Compare with revenue.
Express up-front investment in terms of number of months since costs and revenue have balanced.
This is one of the variables we can tune.
TODO: block reward growth rate can/should be folded into this as an incremental improvement, but ignoring block reward is the right way to get best long-term numbers.
|#

(defparameter *performance-defaults*
  (tuple
   (annual-income 50000.0)
   (aws-glacier-price 0.004)
   (commodity-storage-discount 10)
   (miner-months-to-capacity 3)
   (TiB-drive-cost 30.0)
   (cpu-ghz-cost 10.0)
   (up-front-memory-cost 0.0) ;; FIXME: Incorporate.
   )
  )

(defparameter *isolated-performance-defaults* (tuple (GiB-seal-cycles (* 4.3e9 9042.883))))
(defparameter *integrated-performance-defaults* (tuple (seal-GHz 5)))

(defschema filecoin-price-performance
    "Filecoin price performance."  
  (aws-glacier-price "Cost of one GiB storage from AWS glacier for one month. Unit: dollars")
  (annual-income "Annual income from selling storage on the storage market. Unit: dollars")
  (monthly-income "Monthly income from selling storage on the storage market. Unit: dollars")
  (comparable-monthly-cost "Expected cost of purchasing monthly storage from commodity provider. Unit: dollars")
  (seal-cost "Cost of investment required to seal one GiB in one hour at scale. Unit: dollars")
  (commodity-storage-discount "Fraction of commodity storage pricing expected as income from storage market. Unit: decimal fraction")
  (miner-months-to-capacity "Months it should take a miner to reach full storage capacity. Unit: months")

  (GiB-capacity "GiB of storage at full capacity. Unit: GiB")
  (TiB-capacity "TiB of storage at full capacity. Unit: TiB")
  
  (annual-TiB "TiB of storage which must be brought online per year. Unit: TiB")
  (monthly-TiB "TiB of storage which must be brought online per month. Unit: TiB")
  (daily-TiB "TiB of storage which must be brought online per day. Unit: TiB")
  (hourly-TiB "TiB of storage which must be brought online per hour. Unit: TiB")
  (hourly-GiB "GiB of storage which must be brought online per hour. Unit: GiB")

  (seal-cycles-per-hour "CPU required to seal at required rate for one hour. Unit: cycles")
  (seal-cycles-per-minute "CPU required to seal at required rate for one minute. Unit: cycles")
  (seal-cycles-per-second "CPU required to seal at required rate for one second. Unit: cycles")
  (GiB-seal-cycles "Total CPU cycles required to seal 1 GiB. Unit: cycles")
  (needed-ghz "Total GhZ capacity needed to seal at the required rate.")

  (up-front-drive-cost "Up-front investment in hard drives required to store sufficient sealed data. Unit: dollars.")
  (up-front-memory-cost "Up-front investment in memory (RAM) required to seal at necessary rate. Unit: dollars")
  (up-front-compute-cost "Up-front investement in compute hardware required to seal at necessary rate. Unit: dollars")
  (up-front-sealing-cost "Up-front investment in total hardware require to seal at necessary rate. Unit: dollars")
  (total-up-front-cost "Total up-front investment required to generate MONTHLY-INCOME. Unit: dollars")
  
  (average-monthly-income-during-ramp-up "Average monthly income before miner reaches capacity (assuming linear growth). Unit: dollars")
  (income-during-ramp-up "Total income during ramp-up period (before reaching capacity). Unit: dollars")
  (income-to-fgr-at-capacity "Income still required to reach filecoin growth rate equilibrium after reaching capacity. Unit: dollars")
  (fgr-months-at-capacity "Months needed after reaching capacity before filecoin growth rate equilibrium.")
  (fgr-months "Months after which a miner should reach filecoin growth rate equilibrium.")

  (one-year-profit-months "Months from FGR to one year of profit. Unit: months")
  (one-year-profit "Profit after one year of operation: Unit: dollars")
  (one-year-fgr "FGR after one year of operation: Unit: fraction")
  
  (two-year-profit-months "Months from FGR to two years of profit. Unit: months")
  (two-year-profit "Profit after two years of operation: Unit: dollars")
  (two-year-fgr "FGR after two years of operation: Unit: fraction")
  
  (three-year-profit-months "Months from FGR to three years of profit. Unit: months")
  (three-year-profit "Profit after three years of operation: Unit: dollars")
  (three-year-fgr "FGR after three years of operation: Unit: fraction")
  (seal-Hz "Cycles per second at which the sealing machine operates. Unit: Hz")
  (seal-GHz "Cycles per second at which the sealing machine operates. Unit: GHz")
  )

(defconstraint-system performance-constraint-system    
    ((seal-Hz (* seal-GHz 1e9))
     (GiB-seal-cycles (* GiB-seal-time seal-Hz))
     (monthly-income (/ annual-income 12))
     (monthly-income (* comparable-monthly-cost commodity-storage-discount)) 
     (GiB-capacity (/ comparable-monthly-cost aws-glacier-price))
     (TiB-capacity (/ GiB-capacity 1024))
     (monthly-TiB (/ TiB-capacity miner-months-to-capacity)) 
     (daily-TiB (/ monthly-TiB #.(/ 365 12)))
     (hourly-TiB (/ daily-Tib 24))
     (hourly-GiB (* hourly-TiB 1024)) 
     (up-front-drive-cost (* TiB-drive-cost TiB-capacity))
     (seal-cycles-per-hour (* hourly-GiB GiB-seal-cycles))
     (seal-cycles-per-minute (/ seal-cycles-per-hour 60))
     (seal-cycles-per-second (/ seal-cycles-per-minute 60))
     (needed-GHz (/ seal-cycles-per-second 1e9))
     (up-front-sealing-cost (+ up-front-compute-cost up-front-memory-cost))
     (total-up-front-cost (+ up-front-sealing-cost up-front-drive-cost))
     (up-front-compute-cost (* needed-ghz cpu-ghz-cost))
     (seal-cost (/ up-front-sealing-cost hourly-GiB))
     (average-monthly-income-during-ramp-up (/ monthly-income 2))
     (income-during-ramp-up (* average-monthly-income-during-ramp-up miner-months-to-capacity))
     (income-to-fgr-at-capacity (- total-up-front-cost income-during-ramp-up))
     (fgr-months-at-capacity (/ income-to-fgr-at-capacity monthly-income))
     (fgr-months (+ fgr-months-at-capacity miner-months-to-capacity))

     (one-year-profit-months (- 12 fgr-months))
     (one-year-profit (* one-year-profit-months monthly-income))
     (one-year-fgr (/ one-year-profit total-up-front-cost))

     (two-year-profit-months (- 24 fgr-months))
     (two-year-profit (* two-year-profit-months monthly-income))
     (two-year-fgr (/ two-year-profit total-up-front-cost))

     (three-year-profit-months (- 36 fgr-months))
     (three-year-profit (* three-year-profit-months monthly-income))
     (three-year-fgr (/ three-year-profit total-up-front-cost)))
  :schema 'filecoin-price-performance)

(defun performance-system (&key isolated)
  (make-instance 'system
		 :subsystems (list (find-system 'performance-constraint-system))
		 :data (list *performance-defaults* (if isolated
							*isolated-performance-defaults*
							*integrated-performance-defaults*))))

(test performance-test
  "Test performance system, with default values -- a sanity/regression test for now."
  (is (same (tuple (seal-cost 108.01223))
	    (ask (performance-system :isolated t) '(seal-cost)))))
