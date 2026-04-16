/**
 * processes purchases for a SINGLE patient.
 * must be called separately for each patient.
 * passing purchases from multiple patients will produce incorrect results.
 * 
 * @param {Array} purchases - sorted array of purchases for one patient (ascending by date)
 * @param {Date} studyStartDate - start of the study period
 * @param {Date} studyEndDate - end of the study period
 * @returns {Array} array of usage periods [startDate, endDate, ATCcode, durationDays]
 */

function calculateEndCoverageDate(purchase) {
    const drugName = purchase[1];
    const packageContent = purchase[5];
    if (!purchase[5]) return null;
    const packageContentsRegex = purchase[5].match(/\d+(?=TK|tabl)/g) || [];
    const purchasedDosage = parseFloat(purchase.totalQuantity) || 0;

    // take into account how many days specific purchase covers
    // these assumptions come from drug information leaflet (https://www.ravimiregister.ee/en/default.aspx?pv=HumRavimid.ATCPuu&ot=G&l=en#G) and the code logic assumes full theoretical coverage
    const specificBufferDaysMapping = {
        'G02BA03': {
            '13,5mg 1TK': {
                1: 1095 // up to 3 years (365*3)
            },
            '19,5mg 1TK': {
                1: 1825 // up to 5 years (365*5)
            },
            '52mg 1TK': {
                1: 2190 // up to 6 years (365*6)
            }
        },
        'G02BB01': {
            '11,7mg+2,7mg 1TK': {
                1: 28
            },
            '11,7mg+2,7mg 3TK': {
                3: 84
            },
            '120mcg+15mcg 24h 1TK': {
                1: 28
            }
        },
        'G03AC08': { // implant
            '68mg 1TK': {
                1: 1095
            }
        }
    };

    const genericBufferDaysMapping = {
        3: 28,
        9: 84,
        21: 28, // for example if the package has 21TK, it is still covering 28 days because 7 days are necessary pause
        22: 28,
        28: 28,
        35: 35,
        63: 84,
        66: 84,
        84: 84,
        91: 91,
        105: 105
    };

    // extract number of pills from package contents
    const packageContents = packageContentsRegex.reduce((sum, match) => {
        return sum + parseInt(match, 10);
    }, 0);

    // determine buffer days based on package contents and possibly drug name
    let bufferDays = 0;
    let specificMatchFound = false;

    if (specificBufferDaysMapping[drugName]) {
        for (const key in specificBufferDaysMapping[drugName]) {
            if (packageContent.includes(key) && specificBufferDaysMapping[drugName][key][packageContents]) {
                bufferDays = specificBufferDaysMapping[drugName][key][packageContents] * purchasedDosage;
                specificMatchFound = true;
                break;
            }
        }
    }

    if (!specificMatchFound) {
        bufferDays = (genericBufferDaysMapping[packageContents] || 0) * purchasedDosage;
    }

    if (bufferDays === 0) {
        return null;
    }

    const purchaseDate = new Date(purchase[3]);
    let endCoverageDate = new Date(purchaseDate.getTime() + bufferDays * 86400000);

    return endCoverageDate;
}

function processPurchases(purchases, studyStartDate, studyEndDate) {
    const results = [];
    let currentDrug = null;
    let currentPeriodStart = null;
    let currentEndCoverage = null;
	
	purchases = purchases.map(purchase => ({
        ...purchase,
        totalQuantity: Number(purchase[4])
    }));
	
    purchases.forEach((purchase) => {
        const purchaseDate = new Date(purchase[3]);
        const endCoverageDate = calculateEndCoverageDate(purchase);
        
        if (!endCoverageDate) return;

        if (!currentDrug) {
            currentDrug = purchase[1];
            currentPeriodStart = purchaseDate;
            currentEndCoverage = endCoverageDate;
            return;
        }

        if (purchase[1] === currentDrug) {
            const gapDays = (purchaseDate - currentEndCoverage) / (1000 * 60 * 60 * 24);
            
            if (gapDays < 90) {
            	// if to only bridge please use:
            	//const coverageDuration = endCoverageDate.getTime() - purchaseDate.getTime();
                //currentEndCoverage = new Date(currentEndCoverage.getTime() + coverageDuration);
            	if (endCoverageDate > currentEndCoverage) {
        			// non-overlapping: use actual end of new purchase
        			currentEndCoverage = endCoverageDate;
    			} else {
        			// overlapping or same-day: add coverage duration on top
        			const coverageDuration = endCoverageDate.getTime() - purchaseDate.getTime();
        			currentEndCoverage = new Date(currentEndCoverage.getTime() + coverageDuration);
    			}
            } else {
                results.push([
                    currentPeriodStart.toISOString().slice(0, 10),
                    currentEndCoverage.toISOString().slice(0, 10),
                    currentDrug,
                    Math.round((currentEndCoverage - currentPeriodStart) / (1000 * 60 * 60 * 24))
                ]);
                currentPeriodStart = purchaseDate;
                currentEndCoverage = endCoverageDate;
            }
        } else {
            // different medication - check gap with current end coverage
            if (purchaseDate < currentEndCoverage) {
                // new drug starts before theoretical end - truncate at new drug start
                results.push([
                    currentPeriodStart.toISOString().slice(0, 10),
                    purchaseDate.toISOString().slice(0, 10),
                    currentDrug,
                    Math.round((purchaseDate - currentPeriodStart) / (1000 * 60 * 60 * 24))
                ]);
            } else {
                // new drug starts after theoretical end - use theoretical end
                results.push([
                    currentPeriodStart.toISOString().slice(0, 10),
                    currentEndCoverage.toISOString().slice(0, 10),
                    currentDrug,
                    Math.round((currentEndCoverage - currentPeriodStart) / (1000 * 60 * 60 * 24))
                ]);
            }
            
            // start new drug period
            currentDrug = purchase[1];
            currentPeriodStart = purchaseDate;
            currentEndCoverage = endCoverageDate;
        }
    });

    if (currentDrug) {
        const finalEndDate = currentEndCoverage > studyEndDate ? studyEndDate : currentEndCoverage;
        results.push([
            currentPeriodStart.toISOString().slice(0, 10),
            finalEndDate.toISOString().slice(0, 10),
            currentDrug,
            Math.round((finalEndDate - currentPeriodStart) / (1000 * 60 * 60 * 24))
        ]);
    }

    return results;
}

/* this is the code used to prepare individual's purchases
// sort purchases in sub-array from earliest to latest date
const aggregatedPurchasesPerIndividual = new Map([...purchasesPerIndividual].map(([patientId, purchasesMap]) => {
    const purchasesArray = [...purchasesMap.values()];
    // sort by date after aggregating from earliest to latest
    purchasesArray.sort((a, b) => new Date(a[3]) - new Date(b[3]));
    return [patientId, purchasesArray];
}));

// get purchases array of one individual
let purchases = aggregatedPurchasesPerIndividual.get(patientId) || [];

if (purchases.length === 0) {
        result.push([...row, startStudyDate.toISOString().slice(0, 10), endStudyDate.toISOString().slice(0, 10), "", 0]);
    } else {
        const processedPurchases = processPurchases(purchases, startStudyDate, endStudyDate);
        processedPurchases.forEach(purchase => {
            result.push([...row, ...purchase]);
        });
    }

*/

// purchase array structure:
// [0] id        		- patient identifier
// [1] ATC code         - drug ATC code (e.g. "G03AC08")
// [2] ATC name         - drug active substance name (e.g. "etonogestrel")
// [3] purchase date    - date string (e.g. "2020-03-15")
// [4] total quantity   - dosage (e.g. "2")
// [5] package content  - units per package (e.g. "28TK")

// Scenario 1: Gap < 90 days (merge)
const mockS1 = [
  ["P1", "A00AA01", "DrugA", "2020-03-15", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-06-20", "2", "28TK"],
];

// Scenario 2: Gap > 90 days (two separate periods)
const mockS2 = [
  ["P1", "A00AA01", "DrugA", "2020-03-15", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2021-01-01", "2", "28TK"],
];

// Scenario 3: Drug switch before coverage ends (truncation)
const mockS3 = [
  ["P1", "A00AA01", "DrugA", "2020-03-15", "2", "28TK"],
  ["P1", "A00AA02", "DrugB", "2020-04-01", "2", "28TK"],
];

// Scenario 4: Drug switch after coverage ends
const mockS4 = [
  ["P1", "A00AA01", "DrugA", "2020-03-15", "2", "28TK"],
  ["P1", "A00AA02", "DrugB", "2020-08-01", "2", "28TK"],
];

// Scenario 5: Coverage extends beyond study end date
const mockS5 = [
  ["P1", "A00AA01", "DrugA", "2023-11-15", "2", "28TK"],
];

// S6: Invalid package content (should skip)
const mockS6 = [
  ["P1", "A00AA01", "DrugA", "2020-03-15", "2", "INVALID"],
];

// S7: Three purchases, all within 90 days of each other (should merge into one period)
const mockS7 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-03-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-05-01", "2", "28TK"],
];

// S8: Three purchases, second gap >90 days (should produce two separate periods)
const mockS8 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-03-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2021-01-01", "2", "28TK"],
];

// S9: Drug switch back to original drug (DrugA should appear twice as separate periods)
const mockS9 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA02", "DrugB", "2020-04-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-07-01", "2", "28TK"],
];

// S10a: exactly 89 day gap (should merge)
const mockS10a = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-05-25", "2", "28TK"],
];

// S10b: exactly 90 day gap (should NOT merge)
const mockS10b = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-05-26", "2", "28TK"],
];

// S11: Zero quantity purchase (should skip or handle gracefully)
const mockS11 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "0", "28TK"],
];

// S12: Two different drugs purchased on the same date (first drug gets a 0-day period); handle duplicate-date drug switches upstream
const mockS12 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA02", "DrugB", "2020-01-01", "2", "28TK"],
];

// S13: Same drug purchased on the same date (duplicate purchase)
const mockS13 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
];

// S14: Single package with large quantity
const mockS14 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "10", "28TK"],
];

// S15: Purchases outside study boundaries are not handled ideally; please carefully filter input data
const mockS15 = [
  ["P1", "A00AA01", "DrugA", "2019-11-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-03-01", "2", "28TK"],
];

// S16: Purchases outside study boundaries are not handled ideally; please carefully filter input data
const mockS16 = [
  ["P1", "A00AA01", "DrugA", "2024-01-01", "2", "28TK"],
];

// S17: 21-day drug
const mockS17 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "1", "21TK"]
];

// S18 specific drug case
const mockS18 = [
  ["P1", "G03AC08", "etonogestrel", "2020-01-01", "1", "68mg 1TK"],
];

// S19: Large number of consecutive purchases merging into one long period
const mockS19 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-03-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-05-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-07-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-09-01", "2", "28TK"],
];

// S20: Missing package content (null or undefined)
const mockS20 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", null],
];

// S21: Missing date causes silent failure (defaults to 1970-01-01); filter upstream !!!
const mockS21 = [
  ["P1", "A00AA01", "DrugA", null, "2", "28TK"],
];

// S22: Three drugs on the same date (polypharmacy) produces 0-day periods for all but last drug
const mockS22 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA02", "DrugB", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA03", "DrugC", "2020-01-01", "2", "28TK"],
];

// S23: Drug switch interrupts a previously merged period
const mockS23 = [
  ["P1", "A00AA01", "DrugA", "2020-01-01", "2", "28TK"],
  ["P1", "A00AA01", "DrugA", "2020-03-01", "2", "28TK"], // merges with first DrugA purchase
  ["P1", "A00AA02", "DrugB", "2020-04-01", "2", "28TK"], // interrupts merged DrugA period
];

const studyStart = new Date("2020-01-01");
const studyEnd = new Date("2023-12-31");

console.log("S1:", processPurchases(mockS1, studyStart, studyEnd));
console.log("S2:", processPurchases(mockS2, studyStart, studyEnd));
console.log("S3:", processPurchases(mockS3, studyStart, studyEnd));
console.log("S4:", processPurchases(mockS4, studyStart, studyEnd));
console.log("S5:", processPurchases(mockS5, studyStart, studyEnd));
console.log("S6:", processPurchases(mockS6, studyStart, studyEnd));
console.log("S7:", processPurchases(mockS7, studyStart, studyEnd));
console.log("S8:", processPurchases(mockS8, studyStart, studyEnd));
console.log("S9:", processPurchases(mockS9, studyStart, studyEnd));
console.log("S10a:", processPurchases(mockS10a, studyStart, studyEnd));
console.log("S10b:", processPurchases(mockS10b, studyStart, studyEnd));
console.log("S11:", processPurchases(mockS11, studyStart, studyEnd));
console.warn("S12 [KNOWN LIMITATION - 0-day period]:", processPurchases(mockS12, studyStart, studyEnd));
console.log("S13:", processPurchases(mockS13, studyStart, studyEnd));
console.log("S14:", processPurchases(mockS14, studyStart, studyEnd));
console.warn("S15 [KNOWN LIMITATION - starts before study]:", processPurchases(mockS15, studyStart, studyEnd));
console.warn("S16 [KNOWN LIMITATION - negative duration]:", processPurchases(mockS16, studyStart, studyEnd));
console.log("S17:", processPurchases(mockS17, studyStart, studyEnd));
console.log("S18:", processPurchases(mockS18, studyStart, studyEnd));
console.log("S19:", processPurchases(mockS19, studyStart, studyEnd));
console.log("S20:", processPurchases(mockS20, studyStart, studyEnd));
console.warn("S21 [KNOWN LIMITATION - null date]:", processPurchases(mockS21, studyStart, studyEnd));
console.warn("S22 [KNOWN LIMITATION - polypharmacy same date]:", processPurchases(mockS22, studyStart, studyEnd));
console.log("S23:", processPurchases(mockS23, studyStart, studyEnd));