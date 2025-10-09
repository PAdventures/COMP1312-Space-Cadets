// Imports
import fetchDocument from "./fetchDocument.ts";
import fetchDetails from "./fetchDetails.ts";

// Constants
export const STAFF_LOOKUP_URL = "https://www.ecs.soton.ac.uk/people/";

// Prompt the user for a valid staff email address (or ID)
const emailPrompt = prompt("Enter a valid staff email address (or ID): ");

if (!emailPrompt) {
  console.error("[ERROR] Stdin is not interactive");
  Deno.exit(1);
}

// Create a new email ID variable
let emailId = emailPrompt;

// Check if the user has entered an email address and only collect the email ID
if (emailPrompt.endsWith("@soton.ac.uk")) {
  emailId = emailPrompt.split("@")[0];
}

// Fetch the DOM document
const document = await fetchDocument(emailId);

// Pass the DOM document to fetchDetails
const details = fetchDetails(document);

console.log(`Name: ${details.name}`);
console.log(`Description: ${details.description}`);
console.log(`Faculty: ${details.faculty}`);
console.log(`School: ${details.school}`);
