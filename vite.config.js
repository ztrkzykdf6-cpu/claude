import { defineConfig } from "vite";

const serverUrl = process.env.SERVER_URL ?? "http://localhost:5000";

export default defineConfig({
  root: "src/Client",
  server: {
    port: 8080,
    proxy: {
      "/api": serverUrl,
      "/hubs": { target: serverUrl, ws: true },
    },
    watch: { ignored: ["**/*.fs"] },
  },
  build: {
    outDir: "../Server/wwwroot",
    emptyOutDir: true,
  },
});
