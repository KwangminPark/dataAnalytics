// Preserve the visible transcript. Limit only the context sent with a new question.
export function chatRequest(lab, mode, message, history) {
  const recent = history.slice(-16).map(entry => ({
    role: entry.role,
    content: entry.content.slice(0, 12000),
  }));
  const request = {lab, mode, message, history: recent};
  let trimmed = history.length > 16 || history.slice(-16).some(entry => entry.content.length > 12000);
  const encoder = new TextEncoder();
  while (recent.length && encoder.encode(JSON.stringify(request)).byteLength > 128000) {
    recent.splice(0, 2);
    trimmed = true;
  }
  return {request, trimmed};
}
