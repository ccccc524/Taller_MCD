#######################################################################################
################  Taller Ciencia de Datos ITAM    #####################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Paquetes a utilizar
library(telegram.bot)

#Credenciales
TOKEN    <- "XXXXX" #Token otorgado por Telegram al registrar un bot en @BotFather
PASSWORD <- "XXXXX" #Contraseña del bot - tu la defines

#Sesiones
autenticados <- c()

esta_autenticado <- function(chat_id) {
  as.character(chat_id) %in% autenticados
}

autenticar <- function(chat_id) {
  autenticados <<- unique(c(autenticados, as.character(chat_id)))
}

#Definimos zonas en donde pueden estar los posibles puntos
  #Nota: esto también se puede cargar de un geojson, pero a veces es más ligero definir así los polígonos
delegaciones <- list(
  cuauhtemoc         = list(nombre = "Cuauhtémoc",             lat_min = 19.415, lat_max = 19.455, lon_min = -99.165, lon_max = -99.125),
  miguel_hidalgo     = list(nombre = "Miguel Hidalgo",         lat_min = 19.400, lat_max = 19.470, lon_min = -99.225, lon_max = -99.165),
  benito_juarez      = list(nombre = "Benito Juárez",          lat_min = 19.370, lat_max = 19.415, lon_min = -99.175, lon_max = -99.130),
  coyoacan           = list(nombre = "Coyoacán",               lat_min = 19.320, lat_max = 19.375, lon_min = -99.190, lon_max = -99.115),
  alvaro_obregon     = list(nombre = "Álvaro Obregón",         lat_min = 19.330, lat_max = 19.420, lon_min = -99.290, lon_max = -99.190),
  iztapalapa         = list(nombre = "Iztapalapa",             lat_min = 19.320, lat_max = 19.395, lon_min = -99.115, lon_max = -98.990),
  gustavo_madero     = list(nombre = "Gustavo A. Madero",      lat_min = 19.450, lat_max = 19.560, lon_min = -99.155, lon_max = -99.070),
  azcapotzalco       = list(nombre = "Azcapotzalco",           lat_min = 19.460, lat_max = 19.510, lon_min = -99.220, lon_max = -99.155),
  venustiano_carranza= list(nombre = "Venustiano Carranza",    lat_min = 19.410, lat_max = 19.450, lon_min = -99.120, lon_max = -99.070),
  iztacalco          = list(nombre = "Iztacalco",              lat_min = 19.370, lat_max = 19.410, lon_min = -99.120, lon_max = -99.070),
  tlalpan            = list(nombre = "Tlalpan",                lat_min = 19.150, lat_max = 19.320, lon_min = -99.250, lon_max = -99.120),
  xochimilco         = list(nombre = "Xochimilco",             lat_min = 19.220, lat_max = 19.310, lon_min = -99.150, lon_max = -99.030),
  tlahuac            = list(nombre = "Tláhuac",                lat_min = 19.260, lat_max = 19.340, lon_min = -99.050, lon_max = -98.970),
  magdalena_contras  = list(nombre = "Magdalena Contreras",    lat_min = 19.280, lat_max = 19.360, lon_min = -99.270, lon_max = -99.200),
  cuajimalpa         = list(nombre = "Cuajimalpa",             lat_min = 19.330, lat_max = 19.400, lon_min = -99.340, lon_max = -99.260),
  milpa_alta         = list(nombre = "Milpa Alta",             lat_min = 19.160, lat_max = 19.260, lon_min = -99.050, lon_max = -98.940)
)

generar_destino <- function() {
  zona <- delegaciones[[sample(names(delegaciones), 1)]]
  list(
    lat  = round(runif(1, zona$lat_min, zona$lat_max), 6),
    lon  = round(runif(1, zona$lon_min, zona$lon_max), 6),
    zona = zona$nombre
  )
}

#Comunicación con Bot Telegram
start_handler <- CommandHandler("start", function(bot, update) {
  chat_id <- update$message$chat_id
  
  if (esta_autenticado(chat_id)) {
    bot$sendMessage(
      chat_id    = chat_id,
      text       = paste0(
        "👋 *Bienvenido al Generador de Destinos CDMX*\n\n",
        "Para mandarte a un punto aleatorio necesito tu ubicación actual\\.\n\n",
        "📍 Toca el *clip* \\(adjuntar\\) → *Ubicación* → *Enviar mi ubicación actual*"
      ),
      parse_mode = "Markdown"
    )
  } else {
    bot$sendMessage(
      chat_id    = chat_id,
      text       = paste0(
        "🔒 *Generador de Destinos CDMX*\n\n",
        "Este sistema es de uso exclusivo para personal autorizado\\.\n\n",
        "Por favor ingresa la *contraseña* para continuar:"
      ),
      parse_mode = "Markdown"
    )
  }
})

#Operaciones con acceso concedido al bot
message_handler <- MessageHandler(
  callback = function(bot, update) {
    
    msg     <- update$message
    chat_id <- msg$chat_id
    nombre  <- msg$from$first_name
    
    #Mensaje usuario con autenticación
    if (!esta_autenticado(chat_id)) {
      
      texto_recibido <- trimws(msg$text)
      
      if (!is.null(texto_recibido) && texto_recibido == PASSWORD) {
        autenticar(chat_id)
        bot$sendMessage(
          chat_id    = chat_id,
          text       = paste0(
            "✅ *Acceso concedido\\. Bienvenido, ", nombre, "\\!*\n\n",
            "📍 Para mandarte a un punto aleatorio de la CDMX necesito tu ubicación\\.\n\n",
            "Toca el *clip* \\(adjuntar\\) → *Ubicación* → *Enviar mi ubicación actual*"
          ),
          parse_mode = "Markdown"
        )
      } else {
        bot$sendMessage(
          chat_id    = chat_id,
          text       = paste0(
            "❌ *Contraseña incorrecta\\.* Inténtalo de nuevo\\.\n\n",
            "🔒 Ingresa la contraseña para acceder al sistema:"
          ),
          parse_mode = "Markdown"
        )
      }
      return()
    }
    
    #Flujo del bot:
    
    if (!is.null(msg$location)) {
      
      user_lat <- msg$location$latitude
      user_lon <- msg$location$longitude
      
      bot$sendMessage(
        chat_id    = chat_id,
        text       = paste0("📡 Ubicación recibida, *", nombre, "*\\. Buscando un destino aleatorio\\.\\.\\."),
        parse_mode = "Markdown"
      )
      
      destino <- generar_destino()
      
      maps_url <- paste0(
        "https://www.google.com/maps/dir/",
        user_lat, ",", user_lon, "/",
        destino$lat, ",", destino$lon
      )
      
      mensaje <- paste0(
        "🎲 *DESTINO ALEATORIO ASIGNADO*\n",
        "━━━━━━━━━━━━━━━━━━━━━━\n",
        "📍 Zona: *", destino$zona, "*, CDMX\n",
        "🧭 Coordenadas: `", destino$lat, ", ", destino$lon, "`\n",
        "━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "👇 Toca el botón para abrir tu ruta:"
      )
      
      teclado <- InlineKeyboardMarkup(
        inline_keyboard = list(
          list(
            InlineKeyboardButton(
              text = "🗺️  Abrir ruta en Google Maps",
              url  = maps_url
            )
          )
        )
      )
      
      bot$sendMessage(
        chat_id      = chat_id,
        text         = mensaje,
        parse_mode   = "Markdown",
        reply_markup = teclado
      )
      
    } else {
      bot$sendMessage(
        chat_id    = chat_id,
        text       = paste0(
          "📍 Para continuar necesito tu ubicación\\.\n\n",
          "Toca el *clip* \\(adjuntar\\) → *Ubicación* → *Enviar mi ubicación actual*"
        ),
        parse_mode = "Markdown"
      )
    }
  },
  filters = MessageFilters$all
)

#Iniciamos el Bot
updater <- Updater(token = TOKEN)
updater$dispatcher$add_handler(start_handler)
updater$dispatcher$add_handler(message_handler)

cat("✅ Bot iniciado — esperando mensajes. Ctrl+C para detener.\n")
updater$start_polling(clean = TRUE)
updater$idle()