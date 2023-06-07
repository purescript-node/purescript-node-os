export const getField = (field, err) => err[field];
export const getNullableField = (field, err) => err[field] ? err[field] : null;
