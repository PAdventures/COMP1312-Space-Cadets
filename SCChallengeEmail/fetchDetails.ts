// Imports
import { HTMLDocument } from "@b-fuze/deno-dom";

export default function (document: HTMLDocument) {
  // Find the meta tags for name, description, faculty, and school
  const nameMeta = document.getElementsByTagName("meta").find((element) =>
    element.getAttribute("property") === "og:title"
  );
  const descriptionMeta = document.getElementsByTagName("meta").find((
    element,
  ) => element.getAttribute("name") === "description");
  const facultyMeta = document.getElementsByTagName("meta").find((
    element,
  ) => element.getAttribute("name") === "faculty_metatag");
  const schoolMeta = document.getElementsByTagName("meta").find((
    element,
  ) => element.getAttribute("name") === "school_metatag");

  if (!nameMeta) {
    console.error("[ERROR] Invalid email address or ID");
    Deno.exit(1);
  }

  // Get the name, description, faculty, and school text from the meta tags
  const name = nameMeta.getAttribute("content")?.split(" | ")[0];
  const description = descriptionMeta?.getAttribute("content");
  const faculty = facultyMeta?.getAttribute("content");
  const school = schoolMeta?.getAttribute("content");

  // If the name is "People", it means the ID must be invalid
  if (name === "People") {
    console.error("[ERROR] Invalid email address or ID");
    Deno.exit(1);
  }

  return { name, description, faculty, school };
}
