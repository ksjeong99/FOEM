### Analysis Approach

![alt text](https://github.com/ksjeong99/FOEM/blob/main/FOEM_Dashboard/fig_1.png?raw=true)

### FOEM Framework

![alt text](https://github.com/ksjeong99/FOEM/blob/main/FOEM_Dashboard/fig_2.png?raw=true)

The Freight Operational Efficiency Metrics (FOEM) tool aggregates and reports quantitative freight efficiency metrics for pre-defined future scenarios of U.S. truck freight movement. This framework integrates several National Renewable Energy Laboratory (NREL) tools, including a commercial vehicle stock turnover model (HDStock), the [Transportation Technology Total Cost of Ownership](https://www.nrel.gov/transportation/t3co.html) tool (T3CO), and the [Future Automotive Systems Technology Simulator](https://www.nrel.gov/transportation/fastsim.html) (FASTSim). This integration allows national level estimation of future truck miles of travel by vehicle class, powertrain and model year; fuel consumption per mile based on duty cycle, payload, and vehicle attributes; and levelized total cost of ownership dependent on assumed mileage and lifetime. This integration enables national assessment of energy, cost, and emission metrics at the segment and system level using internally consistent assumptions.

Wherever possible and appropriate, the scenario inputs and results contained in the FOEM are consistent with assumptions used across the 21st Century Truck Partnership analyses. Most significantly, the electrified powertrain scenarios incorporate technical targets established by the Electrification Tech Team (ETT). Vehicle attributes and retail prices were provided by Argonne National Laboratory (ANL) from Autonomie simulations performed for the ETT. These attributes served as inputs to FASTSim and T3CO modeling to analyze the impact on fuel economy of scenario changes to cargo mass and duty cycle.

### Technology Scenarios
Each scenario category includes a baseline, or business as usual (BAU), scenario and at least one alternative future. In general, the BAU scenarios are based on the Energy Information Administration’s (EIA) Annual Energy Outlook (AEO) 2021 reference case, historical data from the 2002 Vehicle Inventory and Use Survey (VIUS), and the Freight Analysis Framework (FAF). It should be noted that, due to the vehicle types currently simulated for the ETT, the FOEM captures only the VMT of diesel vehicles in classes 4-6 and tractor trailers (classes 7&8). The current FOEM baseline accounts for 70% of the total class 4-8 VMT in AEO 2021. In addition, the baseline scenario assumes future new vehicle fuel economy based on attributes provided by ANL. Therefore, the FOEM does not represent the entire medium- and heavy-duty vehicle fleet or EIA’s estimates of future vehicle performance. As a result, total energy and emissions in the baseline scenario will not match published AEO 2021 results.

![alt text](https://github.com/ksjeong99/FOEM/blob/main/FOEM_Dashboard/fig_3.png?raw=true)

Not all of the scenarios shown in the figure above have been implemented in the current version of the FOEM, with grey text indicate those that are not yet included. This section qualitatively describes the scenarios currently available.

+ **Freight Projection**: The BAU freight projection scenario assumes the truck vehicle miles of travel (VMT) demand from the AEO 2021 reference case. The AEO estimates this VMT based on historical relationships between economic activity by sector and truck freight demand, including average payloads from VIUS and FAF. Allocation of VMT to FOEM segments is consistent with the 2002 VIUS. The second scenario represents logistical changes that shift demand from the long-haul segment to the local / regional segment. Because these segments are primarily served by tractors, this scenario does not assume additional truck movements so total VMT remains constant.

+ **Payload Factor**: By using the AEO 2021 VMT projection, the BAU scenario inherently assumes payloads consistent with VIUS and FAF. The second scenario assumes that connectivity and logistics technologies increase average payloads by 10% in all segments. This results in a corresponding 10% reduction in VMT demand and also reduces the number of vehicles needed to move freight.

+ **xEV Powertrain Adoption**: The baseline powertrain adoption scenario assumes the future sales fleet mix found in the AEO 2021 reference case, which includes essentially no battery electric or fuel cell vehicles (BEV or FCHEV) through 2050. 

  Two alternative future projections of the adoption of electrified vehicles are provided. The high zero emission vehicle (ZEV) scenario is an extrapolation of the California Clean Truck mandate applied to national sales. This adoption scenario was also used by the ETT’s Infrastructure Working Group to analyze future infrastructure needs and associated electricity price. This scenario achieves 100% ZEV sales in all truck classes by 2044. The mid case achieves 40% and 50% ZEV sales by 2040 for tractors and non-tractors respectively and continues growing linearly afterward, but does not reach 100% by 2050. 

  All powertrain scenarios apply the diesel and BEV attributes provided by ANL representing electrification component technology targets in 2030 (model year) and extrapolation to 2040. Attributes are then held constant through 2050. The xEV scenarios currently include only diesel and battery electric vehicles. Additional powertrains could be added, such as diesel-electric, plug-in diesel electric, and fuel-cell hybrids, after 21CTP establishes associated technical targets.

  It should be noted that these are “what-if” scenarios and no analysis has been conducted to determine whether the simulated vehicle costs, attributes, and performance will achieve these levels of market acceptance or what economic or policy conditions might be required to achieve them.

+ **Connectivity**: The BAU case assumes no productivity gains as a result of connectivity or automation. The high case assumes that connectivity results in traffic smoothing and eco-routing that reduces the fraction of miles in transient and lower speed conditions. This is currently approximated using three EPA Phase 2 fuel consumption regulatory drive cycles, with the baseline case assuming mileage fractions from those regulations. In the future, this could be updated to use a drive cycle representative of real-world conditions, decomposed into any number of microtrips.

+ **Fuel Prices**: The FOEM currently only includes the AEO 2021 reference case fuel price projection, though electricity price is taken from analysis performed by NREL for the ETT’s Infrastructure working group.

+ **Carbon Emissions**: The FOEM applies well to wheels carbon emissions from ANL’s VISION model, which incorporates ANL GREET model results. The baseline case assumes the national average utility mix and trend, while the high renewables case assumes a 20% reduction in the electricity emission factor by 2040.
