import util from "node:util";
export const getSystemErrorName = (errno) => util.getSystemErrorName(errno);
export const getField = (field, err) => err[field];
export const getNullableField = (field, err) => err[field] ? err[field] : null;
