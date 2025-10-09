// Imports
import { DOMParser } from "@b-fuze/deno-dom";
import { STAFF_LOOKUP_URL } from "./main.ts";

export default async function (emailId: string) {
  // Fetch the HTML data from the staff lookup URL
  const htmlResponse = await fetch(`${STAFF_LOOKUP_URL}${emailId}`, {
    method: "GET",
  });

  // Check if the response is successful
  if (!htmlResponse.ok) {
    console.error(
      "[ERROR] Failed to fetch HTML data, likely due to invalid email address or ID",
    );
    Deno.exit(1);
  }

  // Collect the page text content
  const htmlData = await htmlResponse.text();

  if (!htmlData) {
    console.error("[ERROR] HTML data not found");
    Deno.exit(1);
  }

  // Parse the HTML text data into a DOM document
  const document = new DOMParser().parseFromString(htmlData, "text/html");
  return document;
}
