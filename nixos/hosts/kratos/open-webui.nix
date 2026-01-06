{
  config,
  lib,
  pkgs,
  ...
}:
{
  # This whole LLM thing is useless
  services.open-webui = {
    enable = true;
    host = "127.0.0.1";
    port = 11444;
    openFirewall = false;
    # environmentFile = "/var/lib/secrets/openWebuiSecrets";
    environment = {
      SCARF_NO_ANALYTICS = "True";
      DO_NOT_TRACK = "True";
      ANONYMIZED_TELEMETRY = "False";

      # Perf ----------------------------------
      THREAD_POOL_SIZE = "8";

      # Data ----------------------------------
      ENABLE_ADMIN_EXPORT = "False";

      # Web App ----------------------------------
      WEBUI_AUTH = "False";
      # EXTERNAL_PWA_MANIFEST_URL="" # if set, redirect /manifest.json elsewhere
      ENABLE_TITLE_GENERATION = "True";

      # Local Models ----------------------------------
      USE_CUDA = "False";
      DOCKER = "False";
      USE_CUDA_DOCKER = "False";
      DEVICE_TYPE = "cpu";
      BYPASS_MODEL_ACCESS_CONTROL = "False";

      # Ollama API ----------------------------------
      ENABLE_OLLAMA_API = "True";
      OLLAMA_BASE_URL = "http://127.0.0.1:11434";
      # OLLAMA_BASE_URLS = ["http://127.0.0.1:11434"]
      USE_OLLAMA_DOCKER = "False";
      # K8S_FLAG = "False";

      # OpenAI API --------dd--------------------------
      ENABLE_OPENAI_API = "False";

      # Jupyter ----------------------------------
      # runs python with pyodide
      # downloads pips & peps, req. blacklisting modules
      ENABLE_CODE_EXECUTION = "False";
      ENABLE_CODE_INTERPRETER = "False";

      # Tools ----------------------------------
      ENABLE_DIRECT_CONNECTIONS = "False";

      # Misc ----------------------------------
      ENABLE_AUTOCOMPLETE_GENERATION = "False";
      ENABLE_EVALUATION_ARENA_MODELS = "False";
      ENABLE_COMMUNITY_SHARING = "False";
      ENABLE_TAGS_GENERATION = "True";

      # API ----------------------------------
      ENABLE_API_KEYS = "False";
      # ENABLE_FORWARD_USER_INFO_HEADERS = "True"; # maybe
      # ENABLE_WEB_LOADER_SSL_VERIFICATION = "True";
      # ENABLE_API_KEY_ENDPOINT_RESTRICTIONS = "False";
      # API_KEYS_ALLOWED_ENDPOINTS = "False";
      # JWT_EXPIRES_IN = "1w";
      WEBUI_SESSION_COOKIE_SAME_SITE = "strict";
      # WEBUI_SESSION_COOKIE_SECURE = True; # hmm?
      WEBUI_AUTH_COOKIE_SAME_SITE = "strict";
      # WEBUI_AUTH_COOKIE_SECURE = True; # hmm?

      # Network ----------------------------------
      OFFLINE_MODE = "True"; # prevents model downloads
      # ENABLE_VERSION_UPDATE_CHECK = "False"; # false when OFFLINE_MODE
      # HF_HUB_OFFLINE=0 # disables HF models, sentence xformers, etc
      #   idk whether OFFLINE_MODE implicity disables this
      SAFE_MODE = "True"; # i'll probably regret this
      # CORS_ALLOW_ORIGIN="*"; # this should be set but idk
      # CORS_ALLOW_CUSTOM_SCHEME=""; # didn't know 'bout that

      # RAG Embedding Models -----------------------
      # ... defaults, but still
      RAG_EMBEDDING_MODEL_TRUST_REMOTE_CODE = "False";
      RAG_RERANKING_MODEL_TRUST_REMOTE_CODE = "False";
      RAG_EMBEDDING_MODEL_AUTO_UPDATE = "False";
      RAG_RERANKING_MODEL_AUTO_UPDATE = "False";

      # RAG_EMBEDDING_ENGINE="ollama";
      # RAG_EMBEDDING_ENGINE="SentenceTransformers"; # default
      # RAG_EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2";

      # Vector DBs ----------------------------------
      # VECTOR_DB="pgvector"

      # Web Search ----------------------------------
      ENABLE_WEB_SEARCH = "False";
      # WEB_LOADER_ENGINE="safe_web";
      # ....

      # Database
      # DATABASE_URL = "sqlite:///${DATA_DIR}/webui.db";
      # DATABASE_TYPE = "sqlite+sqlicipher"; # if using encryption, set DATABASE_PASSWORD
      # ... password, host, port, name
    };
  };
}
