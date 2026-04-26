const express = require('express');
const { Pool } = require('pg');
const cors = require('cors');
require('dotenv').config();

const app = express();

// Middlewares
app.use(cors()); // Permite que Angular se conecte
app.use(express.json()); // Permite leer archivos JSON en las peticiones

// Configuración de la conexión a PostgreSQL
const pool = new Pool({
    user: process.env.DB_USER,
    host: process.env.DB_HOST,
    database: process.env.DB_NAME,
    password: process.env.DB_PASSWORD,
    port: process.env.DB_PORT,
});

// Probar conexión
pool.connect((err, client, release) => {
    if (err) {
        return console.error('Error adquiriendo el cliente', err.stack);
    }
    console.log('Conexión a PostgreSQL exitosa');
});


//**********************************SERVICIOS PARA SOCIOS***************************************

//////////////TRAER TODA LA LISTA DE SOCIOS //////////////////////////
app.get('/api/socios', async (req, res) => {
    try {
        // Añadimos ORDER BY idsocio ASC para que siempre sea 01, 02, 03...
        const result = await pool.query('SELECT * FROM socios ORDER BY idsocio ASC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los socios de la base de datos' });
    }
});

//////////////////// REPORTE DE SOCIOS CON CÁLCULOS DE AYUDA, INGRESOS, GASTOS , GANADO Y TOTAL //////////////////////////

// app.get('/api/socios-reporte', async (req, res) => {
//     try {
//         const result = await pool.query('SELECT * FROM socios ORDER BY idsocio ASC');
//         res.status(200).json(result.rows);
//     } catch (err) {
//         console.error(err.message);
//         res.status(500).json({ error: 'Error al obtener los aportes de la base de datos' });
//     }
// });

app.get('/api/socios-reporte', async (req, res) => {
    try {
        const query = `
            SELECT 
                s.idsocio, 
                s.nombresocio,
                -- Conteo de aportes por socio
                COALESCE((SELECT COUNT(*) FROM aportes a WHERE a.idsocio = s.idsocio), 0) as num_aportes,
                -- APORTES REALES (Desde la tabla aportes)
                COALESCE((SELECT SUM(a.aportado) FROM aportes a WHERE a.idsocio = s.idsocio), 0) as aportado,

                -- PRÉSTAMOS (Excluyendo Ayudas)
                COALESCE((SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo NOT LIKE 'AYUDA%'), 0) as montoprestado,

                -- INTERÉS PRESTADO
                COALESCE((SELECT SUM(p.interestotal) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo NOT LIKE 'AYUDA%'), 0) as interesprestado,

                -- CAPITAL PAGADO (Pagos reales, no ingresos/egresos/gst)
                COALESCE((SELECT SUM(pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo NOT LIKE 'INGRESO%' AND pa.tipo NOT LIKE 'EGRESO%' AND pa.numamortizacion != 'GST'), 0) as montopagado,

                -- INTERÉS PAGADO
                COALESCE((SELECT SUM(pa.interes) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo NOT LIKE 'INGRESO%' AND pa.tipo NOT LIKE 'EGRESO%' AND pa.numinteres != 'GST'), 0) as interespagado,

                -- AYUDA (Dinámico)
                -- COALESCE((SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo LIKE 'AYUDA%'), 0) as ayuda,

                -- INGRESOS (Dinámico)
                COALESCE((SELECT SUM(pa.interes + pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo LIKE 'INGRESO%'), 0) as ingresos,

                -- GASTOS (Dinámico)
                -- COALESCE((SELECT SUM(pa.interes + pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo LIKE 'EGRESO%'), 0) as gastos,
                COALESCE(
                    (SELECT SUM(pa.interes + pa.amortizacion) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.tipo LIKE 'EGRESO%'), 
                    0
                ) + 
                COALESCE(
                    (SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo LIKE 'AYUDA%'), 
                    0
                ) -
                 COALESCE(
                    (SELECT SUM(pa.interes) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.numinteres = 'GST'), 
                    0
                ) as gastos,

                -- Mantenemos la columna ayuda por separado si aún la necesitas visualizar sola
                COALESCE(
                    (SELECT SUM(p.montoprestado) FROM prestamos p WHERE p.idsocio = s.idsocio AND p.tipo LIKE 'AYUDA%'), 
                    0
                ) as ayuda,

                -- INTERÉS ANULADO
                COALESCE((SELECT SUM(pa.interes) FROM pagos pa WHERE pa.idsocio = s.idsocio AND pa.numinteres = 'GST'), 0) as interesanulado

            FROM socios s
            ORDER BY s.idsocio ASC
        `;

        const { rows: socios } = await pool.query(query);

        // --- CÁLCULO DE TOTALES GLOBALES (PARA EL ENCABEZADO) ---
        const totalInteresPrestado = socios.reduce((acc, s) => acc + Number(s.interesprestado), 0);
        const totalIngresos = socios.reduce((acc, s) => acc + Number(s.ingresos), 0);
        const totalInteresAnulado = socios.reduce((acc, s) => acc + Number(s.interesanulado), 0);
        const totalGastos = socios.reduce((acc, s) => acc + Number(s.gastos), 0);

        // LOGS DE ENCABEZADOS
        console.log('--- VALORES GLOBALES RECUPERADOS ---');
        console.log(`(+) Total Interés Prestado: ${totalInteresPrestado}`);
        console.log(`(+) Total Ingresos:         ${totalIngresos}`);
        console.log(`(-) Total Interés Anulado:  ${totalInteresAnulado}`);
        console.log(`(-) Total Gastos (Egr+Ayu): ${totalGastos}`);

        // --- CÁLCULO DE UTILIDAD NETA ---
        const utilidadNeta = (totalInteresPrestado + totalIngresos) - (totalInteresAnulado + totalGastos);

        // --- DINAMISMO DE SOCIOS ---
        // Filtramos para excluir cuentas administrativas (01, 02) si no reciben utilidades
        const sociosQueRecibenUtilidad = socios.filter(s => s.idsocio !== '01' && s.idsocio !== '02');
        const cantidadSociosReales = sociosQueRecibenUtilidad.length;

        // LOG DE DINAMISMO
        console.log(`--- DIVISIÓN DINÁMICA ---`);
        console.log(`Socios que participan:      ${cantidadSociosReales}`);

        // Calculamos el ganado por cabeza basado en la cantidad actual
        const ganadoIndividual = cantidadSociosReales > 0 ? (utilidadNeta / cantidadSociosReales) : 0;
        console.log(`(DIV) Ganado por socio:     ${ganadoIndividual.toFixed(2)}`);
        console.log('------------------------------------');

        const respuesta = socios.map(s => {
            const esEspecial = (s.idsocio === '01' || s.idsocio === '02');
            const miGanado = esEspecial ? 0 : ganadoIndividual;

            // Calculamos saldos pendientes aquí mismo para que siempre sean exactos
            const montoPendiente = Number(s.montoprestado) - Number(s.montopagado);
            const interesPendiente = Number(s.interesprestado) - (Number(s.interespagado) + Number(s.interesanulado));

            return {
                ...s,
                montopendiente: Number(montoPendiente.toFixed(2)),
                interespendiente: Number(interesPendiente.toFixed(2)),
                ganado: Number(miGanado.toFixed(2)),
                total: Number((Number(s.aportado) + miGanado).toFixed(2))
            };
        });

        res.json(respuesta);
    } catch (err) {
        console.error("Error en reporte:", err.message);
        res.status(500).send("Error en el servidor");
    }
});

//////////////////////GENERAR EL SIGUIENTE ID PARA NUEVO SOCIO////////////////////////
app.get('/api/socios/siguiente-id', async (req, res) => {
    try {
        // 1. Obtenemos el máximo convirtiendo el string a entero en SQL
        // CAST asegura que '10' sea mayor que '09' numéricamente
        const query = 'SELECT MAX(CAST(idsocio AS INTEGER)) as max_id FROM socios';
        const result = await pool.query(query);

        const ultimoId = result.rows[0].max_id || 0;
        const siguienteNumero = ultimoId + 1;

        // 2. Formatear con ceros a la izquierda (Padding)
        // .padStart(2, '0') convierte 1 en "01", 2 en "02", etc.
        const siguienteId = siguienteNumero.toString().padStart(2, '0');

        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
});

///////////////////////TRAER UN SOCIO POR ID //////////////////////////
app.get('/api/socios/:id', async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM socios WHERE idsocio = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Socio no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
});

////////////////////////// GUARDAR UN NUEVO SOCIO //////////////////////////
app.post('/api/socios', async (req, res) => {
    const { idsocio, nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente } = req.body;

    try {
        const query = `
            INSERT INTO socios (idsocio, nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
            RETURNING *`;

        // Asegúrate de que idsocio llegue como string y no como [object Object]
        const values = [idsocio, nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente];
        const result = await pool.query(query, values);
        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error("Error SQL:", err.detail || err.message);
        res.status(500).json({ error: 'Error al guardar: verifique si el ID ya existe' });
    }
});

////////////////////////// ACTUALIZAR UN SOCIO EXISTENTE //////////////////////////
app.put('/api/socios/:id', async (req, res) => {
    const { id } = req.params;
    const { nombresocio, aportado, montoprestado, montopagado, montopendiente, interesprestado, interespagado, interesanulado, interespendiente } = req.body;

    try {
        const query = `
            UPDATE socios 
            SET nombresocio = $1, 
                aportado = $2, 
                montoprestado = $3, 
                montopagado = $4, 
                montopendiente = $5, 
                interesprestado = $6, 
                interespagado = $7, 
                interesanulado = $8, 
                interespendiente = $9
            WHERE idsocio = $10
            RETURNING *`;

        // AQUÍ ESTABA EL ERROR: Deben ser 6 valores exactamente.
        const values = [
            nombresocio,  // $1
            aportado,     // $2
            montoprestado, // $3
            montopagado,  // $4
            montopendiente, // $5
            interesprestado, // $6
            interespagado,  // $7
            interesanulado, // $8
            interespendiente, // $9
            id            // $10
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Socio no encontrado" });
        }
    } catch (err) {
        console.error("Error al actualizar:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
});

////////////////////////// ELIMINAR UN SOCIO POR ID CON VALIDACIONES DE APORTES Y PRÉSTAMOS //////////////////////////
app.delete('/api/socios/:id', async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Verificar Aportes
        const aportes = await pool.query('SELECT * FROM aportes WHERE idsocio = $1', [id]);
        if (aportes.rows.length > 0) {
            return res.status(400).json({
                message: 'No se puede eliminar: El socio tiene aportes registrados.'
            });
        }

        // 2. Verificar Préstamos
        const prestamos = await pool.query('SELECT * FROM prestamos WHERE idsocio = $1', [id]);
        if (prestamos.rows.length > 0) {
            return res.status(400).json({
                message: 'No se puede eliminar: El socio tiene préstamos pendientes o activos.'
            });
        }

        // Si pasa las validaciones, procedemos
        await pool.query('DELETE FROM socios WHERE idsocio = $1', [id]);
        res.json({ message: 'Socio eliminado con éxito' });
    } catch (err) {
        res.status(500).json({ error: err.message });
    }
});


//**********************************SERVICIOS PARA APORTES***************************************

//////////////TRAER TODA LA LISTA DE APORTES //////////////////////////
app.get('/api/aportes', async (req, res) => {
    try {
        const result = await pool.query('SELECT * FROM aportes ORDER BY fechaaporte DESC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los aportes de la base de datos' });
    }
});

//////////////////////GENERAR EL SIGUIENTE ID PARA NUEVO APORTE////////////////////////
app.get('/api/aportes/siguiente-id', async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idaporte) FROM aportes';
        const result = await pool.query(query);

        // Si no hay aportes, empezamos en 1. Si hay, sumamos 1 al máximo.
        const ultimoId = result.rows[0].max || 0;
        const siguienteId = ultimoId + 1;

        // Devolvemos el número
        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
});

///////////////////////TRAER UN APORTE POR ID //////////////////////////
app.get('/api/aportes/:id', async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM aportes WHERE idaporte = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Aporte no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
});

////////////////////////// GUARDAR UN NUEVO APORTE //////////////////////////
app.post('/api/aportes', async (req, res) => {
    const { idaporte, fechaaporte, aportado, idsocio, nombresocio, comentario } = req.body;

    try {
        // Nos aseguramos de que sea solo la parte de la fecha (YYYY-MM-DD)
        const fechaParaSQL = typeof fechaaporte === 'string'
            ? fechaaporte.split('T')[0]
            : fechaaporte;

        const query = `
            INSERT INTO aportes (idaporte, fechaaporte, aportado, idsocio, nombresocio, comentario)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING *`;

        // Asegúrate de que idsocio llegue como string y no como [object Object]
        const values = [idaporte, fechaParaSQL, aportado, idsocio, nombresocio, comentario];
        const result = await pool.query(query, values);

        // 2. RECALCULAR
        await actualizarTodoElSocio(req.body.idsocio); // <--- RECALCULAR

        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error("Error SQL:", err.detail || err.message);
        res.status(500).json({ error: 'Error al guardar: verifique si el ID ya existe' });
    }
});

////////////////////////// ACTUALIZAR UN APORTE EXISTENTE //////////////////////////
app.put('/api/aportes/:id', async (req, res) => {
    const { id } = req.params;
    const { fechaaporte, aportado, idsocio, nombresocio, comentario } = req.body;

    try {
        // Nos aseguramos de que sea solo la parte de la fecha (YYYY-MM-DD)
        const fechaParaSQL = typeof fechaaporte === 'string'
            ? fechaaporte.split('T')[0]
            : fechaaporte;
        const query = `
            UPDATE aportes 
            SET fechaaporte = $1, 
                aportado = $2, 
                idsocio = $3, 
                nombresocio = $4, 
                comentario = $5
            WHERE idaporte = $6
            RETURNING *`;

        // AQUÍ ESTABA EL ERROR: Deben ser 6 valores exactamente.
        const values = [
            fechaParaSQL, // $1
            aportado,     // $2
            idsocio,      // $3
            nombresocio,  // $4
            comentario,   // $5 (Este faltaba en tu arreglo anterior)
            id            // $6
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            // RECALCULAR después de actualizar el monto
            await actualizarTodoElSocio(req.body.idsocio);
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Aporte no encontrado" });
        }
    } catch (err) {
        console.error("Error al actualizar:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
});

/////////////////////////////// ELIMINAR UN APORTE POR ID //////////////////////////
app.delete('/api/aportes/:id', async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Buscamos el idsocio antes de borrar para no perder la referencia
        const buscarSocio = await pool.query('SELECT idsocio FROM aportes WHERE idaporte = $1', [id]);

        if (buscarSocio.rows.length > 0) {
            const idsocio = buscarSocio.rows[0].idsocio;

            // 2. Borramos el aporte
            await pool.query('DELETE FROM aportes WHERE idaporte = $1', [id]);

            // 3. RECALCULAR el nuevo total (ya sin el registro borrado)
            await actualizarTodoElSocio(req.body.idsocio);

            res.json({ message: "Eliminado correctamente y saldo actualizado" });
        } else {
            res.status(404).json({ message: "Aporte no encontrado" });
        }
    } catch (err) {
        console.error("Error al eliminar:", err);
        res.status(500).send("Error al eliminar");
    }
});


//**********************************SERVICIOS PARA PRESTAMOS***************************************

////////////////////// OBTENER TODA LA LISTA DE PRESTAMOS //////////////////////////
// app.get('/api/prestamos', async (req, res) => {
//     try {
//         const result = await pool.query('SELECT * FROM prestamos');
//         res.status(200).json(result.rows);
//     } catch (err) {
//         console.error(err.message);
//         res.status(500).json({ error: 'Error al obtener los prestamos de la base de datos' });
//     }
// });

app.get('/api/prestamos', async (req, res) => {
    try {
        const query = `
            SELECT 
                p.*,
                -- LÓGICA DE ESTATUS MEJORADA
                CASE 
                    -- Si es Ayuda, marcar siempre como PAGADO
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'PAGADO'

                    -- Si es Préstamo, calcular según el saldo
                    WHEN (
                        p.montoprestado + p.interestotal - 
                        COALESCE((SELECT SUM(amortizacion + interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0)
                    ) <= 0 THEN 'PAGADO'

                    ELSE 'PENDIENTE'
                END AS estatus_dinamico,

                -- LÓGICA DE LETRAS
                CASE 
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'N/A' -- Las ayudas no tienen cuotas
                    ELSE (
                        p.plazoprestamo - 
                        COALESCE((SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos AND pg.numamortizacion != 'EGR'), 0)
                    ) || ' / ' || p.plazoprestamo
                END AS letras_resumen
            FROM prestamos p
            ORDER BY p.fechaprestamo DESC
        `;
        const result = await pool.query(query);
        res.json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: "Error al obtener préstamos" });
    }
});

////////////////////////// GENERAR EL SIGUIENTE ID PARA NUEVO PRESTAMO (PRESTAMO o AYUDA) //////////////////////////
app.get('/api/prestamos/siguiente-id', async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idprestamos) FROM prestamos';
        const result = await pool.query(query);

        // Si no hay aportes, empezamos en 1. Si hay, sumamos 1 al máximo.
        const ultimoId = result.rows[0].max || 0;
        const siguienteId = ultimoId + 1;

        // Devolvemos el número
        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
});

// app.get('/api/prestamos/siguiente-id/:tipo', async (req, res) => {
//     const { tipo } = req.params; // Capturamos si es PRESTAMO o AYUDA

//     try {
//         let query;
//         let esAyuda = (tipo === 'AYUDA');

//         if (!esAyuda) {
//             // Caso PRESTAMO: Buscamos el número más alto ignorando los que tienen letras
//             // La regex '^[0-9]+$' filtra registros que solo contienen dígitos
//             query = `
//                 SELECT idprestamos 
//                 FROM prestamos 
//                 WHERE idprestamos ~ '^[0-9]+$' 
//                 ORDER BY idprestamos::INTEGER DESC 
//                 LIMIT 1`;
//         } else {
//             // Caso AYUDA: Buscamos el que tenga el número más alto después del prefijo 'AYUDA'
//             query = `
//                 SELECT idprestamos
//                 FROM prestamos 
//                 WHERE idprestamos LIKE 'AYUDA%' 
//                 ORDER BY LENGTH(idprestamos) DESC, idprestamos DESC 
//                 LIMIT 1`;
//         }

//         const result = await pool.query(query);
//         let siguienteId;

//         if (result.rows.length === 0) {
//             // Si la tabla está vacía para ese tipo
//             siguienteId = esAyuda ? "AYUDA1" : "1";
//         } else {
//             const ultimoId = result.rows[0].idprestamos;

//             if (!esAyuda) {
//                 // Incremento numérico simple
//                 siguienteId = (parseInt(ultimoId) + 1).toString();
//             } else {
//                 // Extraemos el número, incrementamos y volvemos a poner el prefijo
//                 const numeroActual = parseInt(ultimoId.replace('AYUDA', ''));
//                 siguienteId = `AYUDA${numeroActual + 1}`;
//             }
//         }

//         res.json({ siguienteId });

//     } catch (err) {
//         console.error("Error en siguiente-id:", err);
//         res.status(500).json({ error: 'Error al calcular el siguiente ID' });
//     }
// });

///////////////////////////// OBTENER LA LISTA DE PRESTAMOS PENDIENTES (EXCLUYENDO AYUDA) //////////////////////////
app.get('/api/prestamos/pendientes', async (req, res) => {
    try {
        const query = `
        SELECT 
            p.idprestamos, 
            s.nombresocio,
            s.idsocio,
            CAST(p.amortizacion AS FLOAT) AS amortizacion_sugerida,
            CAST(p.interesmensual AS FLOAT) AS interes_sugerido,
            (SELECT COUNT(*) FROM pagos WHERE idprestamos = p.idprestamos AND amortizacion > 0) + 1 AS num_amortizacion,
            (SELECT COUNT(*) FROM pagos WHERE idprestamos = p.idprestamos AND interes > 0) + 1 AS num_interes
        FROM prestamos p
        JOIN socios s ON p.idsocio = s.idsocio
        WHERE p.tipo NOT LIKE 'AYUDA%' 
        AND (
            -- Condición 1: Todavía debe capital
            p.montoprestado > (
                SELECT COALESCE(SUM(amortizacion), 0) 
                FROM pagos 
                WHERE idprestamos = p.idprestamos
            )
            OR 
            -- Condición 2: Todavía debe intereses
            p.interestotal > (
                SELECT COALESCE(SUM(interes), 0) 
                FROM pagos 
                WHERE idprestamos = p.idprestamos
            )
        )
        ORDER BY CAST(p.idprestamos AS INTEGER) ASC;
    `;

        const result = await pool.query(query);
        res.json(result.rows || []);
    } catch (err) {
        console.error("Error en pendientes-lista:", err);
        res.status(500).json({ error: 'Error interno al obtener la lista de pendientes' });
    }
});

//////////////////////////////////// OBTENER DETALLES DE UN PRESTAMO PENDIENTE //////////////////////////
// En tu index.js (Node.js)
// app.get('/api/prestamos/detalles/:idprestamos', async (req, res) => {
//     try {
//         const { idprestamos } = req.params;
//         const query = `
//             SELECT 
//                 p.montoprestado, 
//                 p.fechaprestamo, 
//                 p.plazoprestamo, 
//                 p.interestotal,
//                 -- LA CUOTA ES: (Capital / Plazo) + (Interés Total / Plazo)
//                 CASE 
//                     WHEN p.plazoprestamo > 0 THEN 
//                         (p.montoprestado / p.plazoprestamo) + (p.interestotal / p.plazoprestamo)
//                     ELSE 
//                         p.montoprestado + p.interestotal 
//                 END as cuotaprestamo,

//                 (SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as letras_pagadas,
//                 p.montoprestado - COALESCE((SELECT SUM(amortizacion) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0) as amortizacion_pendiente,
//                 p.interestotal - COALESCE((SELECT SUM(interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0) as interes_pendiente
//             FROM prestamos p
//             WHERE p.idprestamos = $1`;

//         const result = await pool.query(query, [idprestamos]);

//         if (result.rows.length > 0) {
//             const data = result.rows[0];
//             const letrasPagadas = parseInt(data.letras_pagadas);

//             res.json({
//                 montoPrestado: data.montoprestado,
//                 fechaPrestamo: data.fechaprestamo,
//                 plazoPrestamo: data.plazoprestamo,
//                 cuotaPrestamo: data.cuotaprestamo,
//                 letrasPagadas: letrasPagadas,
//                 // Cálculo de pendientes basado en el total de registros encontrados
//                 letrasPendientes: Math.max(0, data.plazoprestamo - letrasPagadas),
//                 amortizacionPendiente: Math.max(0, data.amortizacion_pendiente),
//                 interesPendiente: Math.max(0, data.interes_pendiente)
//             });
//         } else {
//             res.status(404).send("Préstamo no encontrado");
//         }
//     } catch (err) {
//         console.error(err.message);
//         res.status(500).send("Error al obtener detalles del préstamo");
//     }
// });
app.get('/api/prestamos/detalles/:idprestamos', async (req, res) => {
    try {
        const { idprestamos } = req.params;
        const query = `
            SELECT 
                p.idprestamos, p.montoprestado, p.fechaprestamo, 
                p.plazoprestamo, p.interestotal, p.comentario, p.interesprestamo,
                (SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as letras_pagadas,
                (SELECT SUM(amortizacion) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as sum_amortizacion,
                (SELECT SUM(interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos) as sum_interes
            FROM prestamos p
            WHERE p.idprestamos = $1`;

        const result = await pool.query(query, [idprestamos]);
        if (result.rows.length === 0) return res.status(404).send("Préstamo no encontrado");

        const p = result.rows[0];

        // --- DATOS BASE ---
        const montoInicial = parseFloat(p.montoprestado);
        const plazoTotal = parseInt(p.plazoprestamo);
        const interesTotalDefinido = parseFloat(p.interestotal);
        const tasaMensual = parseFloat(p.interesprestamo) / 100;
        const tipoAmortizacion = (p.comentario || "").toUpperCase().trim();

        // --- DETECTAR LA LETRA ACTUAL ---
        const letrasPagadas = parseInt(p.letras_pagadas);
        const nSiguiente = letrasPagadas + 1; // La letra que se va a pagar ahora

        let proximaAmortizacion = 0;
        let proximoInteres = 0;

        // --- LÓGICA POR TIPO DE SISTEMA ---

        if (tipoAmortizacion.includes("ALEMANA")) {
            // ALEMÁN: Amortización de capital es SIEMPRE fija
            proximaAmortizacion = montoInicial / plazoTotal;
            // El interés se calcula sobre el saldo de capital que queda
            // Saldo = MontoInicial - (AmortizaciónFija * letras ya pagadas)
            const saldoAlInicioDeEstaLetra = montoInicial - (proximaAmortizacion * letrasPagadas);
            proximoInteres = saldoAlInicioDeEstaLetra * tasaMensual;
            console.log(`(ALEMANA) Letra ${nSiguiente}: Amortización fija de ${proximaAmortizacion.toFixed(2)}, Interés sobre saldo ${saldoAlInicioDeEstaLetra.toFixed(2)} = ${proximoInteres.toFixed(2)}`);
        }
        else if (tipoAmortizacion.includes("FRANCESA")) {
            // FRANCÉS: Cuota total es fija, pero el desglose cambia cada mes
            const cuotaFija = (montoInicial + interesTotalDefinido) / plazoTotal;

            // Simulamos la tabla hasta llegar a la letra nSiguiente
            let saldoTemporal = montoInicial;
            for (let i = 1; i <= nSiguiente; i++) {
                proximoInteres = saldoTemporal * tasaMensual;
                proximaAmortizacion = cuotaFija - proximoInteres;
                saldoTemporal -= proximaAmortizacion;
            }
            console.log(`(FRANCESA) Letra ${nSiguiente}: Cuota fija de ${cuotaFija.toFixed(2)}, Amortización ${proximaAmortizacion.toFixed(2)}, Interés ${proximoInteres.toFixed(2)}, Saldo restante ${saldoTemporal.toFixed(2)}`);
        }
        else {
            // FAMILIAR / LINEAL / DEFAULT: Todo se divide para el plazo
            proximaAmortizacion = montoInicial / plazoTotal;
            proximoInteres = interesTotalDefinido / plazoTotal;
            console.log(`(FAMILIAR/LINEAL) Letra ${nSiguiente}: Amortización ${proximaAmortizacion.toFixed(2)}, Interés ${proximoInteres.toFixed(2)}`);
        }

        // --- PROTECCIÓN FIN DE PLAZO ---
        if (nSiguiente > plazoTotal) {
            proximaAmortizacion = 0;
            proximoInteres = 0;
        }

        // --- RESPUESTA PARA EL FRONTEND ---
        res.json({
            montoPrestado: montoInicial,
            fechaPrestamo: p.fechaprestamo,
            plazoPrestamo: plazoTotal,
            tipoAmortizacion: tipoAmortizacion,
            // Valores que cargan los inputs de Amortización e Interés
            proximaAmortizacion: parseFloat(proximaAmortizacion.toFixed(2)),
            proximoInteres: parseFloat(proximoInteres.toFixed(2)),
            cuotaPrestamo: parseFloat((proximaAmortizacion + proximoInteres).toFixed(2)),
            // Información para etiquetas y contadores
            numLetra: nSiguiente,
            letrasPagadas: letrasPagadas,
            letrasPendientes: Math.max(0, plazoTotal - letrasPagadas),
            amortizacionPendiente: (montoInicial - (parseFloat(p.sum_amortizacion) || 0)).toFixed(2),
            interesPendiente: (interesTotalDefinido - (parseFloat(p.sum_interes) || 0)).toFixed(2)
        });

    } catch (err) {
        console.error("Error:", err.message);
        res.status(500).send("Error interno al calcular detalles");
    }
});

/////////////////////////// OBTENER UN PRESTAMO POR ID //////////////////////////
app.get('/api/prestamos/:id', async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM prestamos WHERE idprestamos = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Préstamo no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
});

/////////////////////////// OBTENER LOS PRESTAMOS POR ID SOCIO //////////////////////////
// app.get('/api/prestamos/socio/:idsocio', async (req, res) => {
//     const { idsocio } = req.params;
//     try {
//         // Obtenemos todos los préstamos asociados a ese ID de socio
//         const result = await pool.query(

//             'SELECT * FROM prestamos WHERE idsocio = $1 ORDER BY fechaprestamo ASC',
//             [idsocio]
//         );

//         // Enviamos todo el arreglo (vacío o con datos)
//         // Es mejor devolver [] que un error 404 si el socio simplemente no tiene préstamos aún
//         res.json(result.rows);

//     } catch (err) {
//         console.error(err.message);
//         res.status(500).send("Error en el servidor al obtener préstamos del socio");
//     }
// });
/////////////////////////// OBTENER LOS PRESTAMOS POR ID SOCIO //////////////////////////
app.get('/api/prestamos/socio/:idsocio', async (req, res) => {
    const { idsocio } = req.params;
    try {
        const result = await pool.query(
            `SELECT 
                p.*,
                -- LÓGICA DE ESTATUS MEJORADA
                CASE 
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'PAGADO'
                    WHEN (
                        p.montoprestado + p.interestotal - 
                        COALESCE((SELECT SUM(amortizacion + interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0)
                    ) <= 0 THEN 'PAGADO'
                    ELSE 'PENDIENTE'
                END AS estatus_dinamico,

                -- LÓGICA DE LETRAS
                CASE 
                    WHEN p.tipo LIKE 'AYUDA%' THEN 'N/A'
                    ELSE (
                        p.plazoprestamo - 
                        COALESCE((SELECT COUNT(*) FROM pagos pg WHERE pg.idprestamos = p.idprestamos AND pg.numamortizacion != 'EGR'), 0)
                    ) || ' / ' || p.plazoprestamo
                END AS letras_resumen
            FROM prestamos p
            WHERE p.idsocio = $1
            ORDER BY p.fechaprestamo ASC`,
            [idsocio]
        );

        res.json(result.rows);

    } catch (err) {
        console.error("Error detalle:", err.message);
        res.status(500).send("Error en el servidor al obtener préstamos del socio");
    }
});

///////////////////////////// GUARDAR UN NUEVO PRESTAMO //////////////////////////
app.post('/api/prestamos', async (req, res) => {
    // 1. Agregamos 'tipo' a la desestructuración del body
    const {
        idprestamos, tipo, idsocio, nombresocio, fechaprestamo,
        montoprestado, plazoprestamo, interesprestamo, interesmensual,
        interestotal, amortizacion, cuota, total, comentario
    } = req.body;

    try {
        // 2. Agregamos 'tipo' a la consulta SQL y un nuevo marcador $14
        const query = `
            INSERT INTO prestamos (
                idprestamos, tipo, idsocio, nombresocio, fechaprestamo, 
                montoprestado, plazoprestamo, interesprestamo, interesmensual, 
                interestotal, amortizacion, cuota, total, comentario
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
            RETURNING *`;

        // 3. Incluimos 'tipo' en el array de valores (asegúrate que el orden coincida con el INSERT)
        const values = [
            idprestamos,
            tipo || 'PRESTAMO', // Salvaguarda por si llega vacío
            idsocio,
            nombresocio,
            fechaprestamo,
            montoprestado,
            plazoprestamo,
            interesprestamo,
            interesmensual,
            interestotal,
            amortizacion,
            cuota,
            total,
            comentario
        ];

        const result = await pool.query(query, values);

        // Recalcular datos del socio (Muy bien que lo tengas así)
        await actualizarTodoElSocio(idsocio);

        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error("Error en POST /api/prestamos:", err.detail || err.message);
        res.status(500).json({
            error: 'Error al insertar el registro',
            detalle: err.detail
        });
    }
});

//////////////////////////////// ACTUALIZAR UN PRESTAMO EXISTENTE //////////////////////////
// app.put('/api/prestamos/:id', async (req, res) => {
//     const { id } = req.params; // Este es el ID que viene en la URL
//     const {
//         idsocio, nombresocio, fechaprestamo, montoprestado, plazoprestamo,
//         interesprestamo, interesmensual, interestotal, amortizacion,
//         cuota, total, comentario
//     } = req.body;

//     try {
//         // 1. Cambiamos la tabla a 'prestamos'
//         // 2. Mapeamos cada columna con su respectivo marcador ($1, $2, etc.)
//         const query = `
//             UPDATE prestamos 
//             SET idsocio = $1, 
//                 nombresocio = $2, 
//                 fechaprestamo = $3, 
//                 montoprestado = $4, 
//                 plazoprestamo = $5, 
//                 interesprestamo = $6, 
//                 interesmensual = $7, 
//                 interestotal = $8, 
//                 amortizacion = $9, 
//                 cuota = $10, 
//                 total = $11, 
//                 comentario = $12
//             WHERE idprestamos = $13
//             RETURNING *`;

//         // Los valores deben seguir EXACTAMENTE el orden del query arriba ($1 a $13)
//         const values = [
//             idsocio,          // $1
//             nombresocio,      // $2
//             fechaprestamo,    // $3
//             montoprestado,    // $4
//             plazoprestamo,    // $5
//             interesprestamo,  // $6
//             interesmensual,   // $7
//             interestotal,     // $8
//             amortizacion,     // $9
//             cuota,            // $10
//             total,            // $11
//             comentario,       // $12
//             id                // $13 (El ID que viene de la URL)
//         ];

//         const result = await pool.query(query, values);

//         if (result.rows.length > 0) {
//             await actualizarTodoElSocio(req.body.idsocio); // <--- RECALCULAR
//             res.json(result.rows[0]);
//         } else {
//             res.status(404).json({ message: "Préstamo no encontrado por id" });
//         }
//     } catch (err) {
//         console.error("Error al actualizar préstamo:", err);
//         res.status(500).json({ error: "Error interno del servidor al actualizar" });
//     }
// });
app.put('/api/prestamos/:id', async (req, res) => {
    const { id } = req.params;
    const {
        idsocio, nombresocio, fechaprestamo, montoprestado, plazoprestamo,
        interesprestamo, interesmensual, interestotal, amortizacion,
        cuota, total, comentario, tipo // Asegúrate de recibir 'tipo' para la validación
    } = req.body;

    try {
        // --- 1. VALIDACIÓN DE ESTATUS DINÁMICO ANTES DE ACTUALIZAR ---
        const checkStatusQuery = `
            SELECT 
                p.tipo,
                (
                    p.montoprestado + p.interestotal - 
                    COALESCE((SELECT SUM(amortizacion + interes) FROM pagos pg WHERE pg.idprestamos = p.idprestamos), 0)
                ) AS saldo_pendiente
            FROM prestamos p 
            WHERE p.idprestamos = $1`;

        const checkResult = await pool.query(checkStatusQuery, [id]);

        if (checkResult.rows.length === 0) {
            return res.status(404).json({ message: "Préstamo no encontrado" });
        }

        const prestamoActual = checkResult.rows[0];
        const esAyuda = prestamoActual.tipo && prestamoActual.tipo.startsWith('AYUDA');
        const estaPagado = parseFloat(prestamoActual.saldo_pendiente) <= 0;

        // Bloqueamos si es AYUDA o si el saldo es 0 (PAGADO)
        if (esAyuda || estaPagado) {
            return res.status(400).json({
                error: "BLOQUEADO",
                message: "No se puede editar un préstamo con estatus PAGADO o tipo AYUDA."
            });
        }

        // --- 2. EJECUCIÓN DEL UPDATE (Si pasa la validación) ---
        const query = `
            UPDATE prestamos 
            SET idsocio = $1, nombresocio = $2, fechaprestamo = $3, 
                montoprestado = $4, plazoprestamo = $5, interesprestamo = $6, 
                interesmensual = $7, interestotal = $8, amortizacion = $9, 
                cuota = $10, total = $11, comentario = $12
            WHERE idprestamos = $13
            RETURNING *`;

        const values = [
            idsocio, nombresocio, fechaprestamo, montoprestado, plazoprestamo,
            interesprestamo, interesmensual, interestotal, amortizacion,
            cuota, total, comentario, id
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            // Recalculamos totales del socio (tu función existente)
            await actualizarTodoElSocio(idsocio);
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Error al actualizar: Registro no encontrado" });
        }

    } catch (err) {
        console.error("Error al actualizar préstamo:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
});

/////////////////////////////// ELIMINAR UN PRESTAMO POR ID //////////////////////////
app.delete('/api/prestamos/:id', async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Verificar si el préstamo tiene pagos
        const consultaPagos = await pool.query('SELECT * FROM pagos WHERE idprestamos = $1', [id]);

        if (consultaPagos.rows.length > 0) {
            return res.status(400).json({
                error: 'Restricción de Integridad',
                message: `No se puede eliminar el préstamo #${id} porque ya tiene pagos registrados.`
            });
        }

        // 2. OBTENER EL IDSOCIO antes de borrar (Esto lo tienes bien)
        const datosPrestamo = await pool.query('SELECT idsocio FROM prestamos WHERE idprestamos = $1', [id]);

        if (datosPrestamo.rows.length === 0) {
            return res.status(404).json({ message: 'Préstamo no encontrado' });
        }

        const idsocioFinal = datosPrestamo.rows[0].idsocio;

        // 3. Proceder a borrar
        await pool.query('DELETE FROM prestamos WHERE idprestamos = $1', [id]);

        // 4. USAR LA VARIABLE idsocioFinal (NO req.body)
        await actualizarTodoElSocio(idsocioFinal);

        res.json({ message: 'Registro eliminado correctamente y saldo de préstamos actualizado' });

    } catch (err) {
        console.error("Error al eliminar préstamo:", err);
        res.status(500).json({ error: 'Error interno del servidor' });
    }
});

//**********************************SERVICIOS PARA PAGOS***************************************

//////////////TRAER TODA LA LISTA DE PAGOS //////////////////////////
app.get('/api/pagos', async (req, res) => {
    try {
        const result = await pool.query('SELECT * FROM pagos  ORDER BY fechaamortizacion DESC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los pagos de la base de datos' });
    }
});

///////////////////////////////// GENERAR EL SIGUIENTE ID PARA PAGOS (PAGO, INGRESO, EGRESO) //////////////////////////
app.get('/api/pagos/siguiente-id', async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idpagos) FROM pagos';
        const result = await pool.query(query);

        // Si no hay aportes, empezamos en 1. Si hay, sumamos 1 al máximo.
        const ultimoId = result.rows[0].max || 0;
        const siguienteId = ultimoId + 1;

        // Devolvemos el número
        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
});
// app.get('/api/pagos/siguiente-id/:tipo', async (req, res) => {
//     const { tipo } = req.params; // Capturamos PAGO, INGRESO o EGRESO

//     try {
//         let query;
//         let values = [];

//         if (tipo === 'PAGO') {
//             // Caso PAGO: Solo números (1, 2, 3...)
//             query = `
//                 SELECT idpagos 
//                 FROM pagos 
//                 WHERE idpagos ~ '^[0-9]+$' 
//                 ORDER BY idpagos::INTEGER DESC 
//                 LIMIT 1`;
//         } else {
//             // Caso INGRESO o EGRESO: Buscamos por el prefijo correspondiente
//             query = `
//                 SELECT idpagos
//                 FROM pagos 
//                 WHERE idpagos LIKE $1 
//                 ORDER BY LENGTH(idpagos) DESC, idpagos DESC 
//                 LIMIT 1`;
//             values = [`${tipo}%`];
//         }

//         const result = await pool.query(query, values);
//         let siguienteId;

//         if (result.rows.length === 0) {
//             // Si no hay registros previos, inicializamos según el tipo
//             siguienteId = (tipo === 'PAGO') ? "1" : `${tipo}1`;
//         } else {
//             // ERROR CORREGIDO: Usar idpagos, no idprestamos
//             const ultimoId = result.rows[0].idpagos;

//             if (tipo === 'PAGO') {
//                 siguienteId = (parseInt(ultimoId) + 1).toString();
//             } else {
//                 // Extraemos el número después del texto (ej: INGRESO1 -> 1)
//                 const numeroActual = parseInt(ultimoId.replace(tipo, '')) || 0;
//                 siguienteId = `${tipo}${numeroActual + 1}`;
//             }
//         }

//         res.json({ siguienteId });

//     } catch (err) {
//         console.error("Error en siguiente-id-pagos:", err);
//         res.status(500).json({ error: 'Error al calcular el siguiente ID' });
//     }
// });

///////////////////////////// GUARDAR UN NUEVO PAGO //////////////////////////
app.post('/api/pagos/guardar', async (req, res) => {
    const {
        idpagos, tipo, idsocio, nombresocio, idprestamos,
        amortizacion, fechaamortizacion, numamortizacion,
        interes, fechainteres, numinteres, comentario
    } = req.body;

    // 1. Manejo de idprestamos para evitar errores de FK
    const idPrestamosFinal = (idprestamos === '0' || idprestamos === 0 || idprestamos === '' || !idprestamos)
        ? null
        : idprestamos;

    // 2. Asegurar que los montos no sean undefined/null para evitar errores en la BD
    const amortizacionFinal = amortizacion || 0;
    const interesFinal = interes || 0;

    try {
        const query = `
            INSERT INTO pagos (
                idpagos, tipo, idsocio, nombresocio, idprestamos, 
                amortizacion, fechaamortizacion, numamortizacion, 
                interes, fechainteres, numinteres, comentario
            ) 
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            RETURNING *`;

        const values = [
            idpagos, tipo, idsocio, nombresocio, idPrestamosFinal,
            amortizacionFinal, fechaamortizacion, numamortizacion,
            interesFinal, fechainteres, numinteres, comentario
        ];

        const result = await pool.query(query, values);
        await actualizarTodoElSocio(req.body.idsocio); // <--- RECALCULAR
        res.status(201).json(result.rows[0]);

    } catch (err) {
        console.error('Error en el servidor:', err);
        // Enviamos el mensaje de error real para debuguear más rápido
        res.status(500).json({ error: 'Error al insertar el registro', detail: err.message });
    }
});

/////////////////////////// OBTENER UN PAGO POR ID //////////////////////////
app.get('/api/pagos/:id', async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM pagos WHERE idpagos = $1 ORDER BY fechaamortizacion ASC', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Pago no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
});

/////////////////////////// OBTENER LOS PAGOS POR ID SOCIO //////////////////////////
// app.get('/api/pagos/socio/:idsocio', async (req, res) => {
//     const { idsocio } = req.params;
//     try {
//         // Cambiamos fechapago por idprestamos
//         const query = 'SELECT * FROM pagos WHERE idsocio = $1 ORDER BY fechaamortizacion ASC';
//         const result = await pool.query(query, [idsocio]);

//         res.json(result.rows);
//     } catch (err) {
//         console.error("Error en el servidor:", err.message);
//         res.status(500).json({ error: "Error al obtener pagos ordenados por préstamo" });
//     }
// });

// app.get('/api/pagos/socio/:idsocio', async (req, res) => {
//     const { idsocio } = req.params;
//     try {
//         const query = `
//             SELECT pg.*, pr.tipo 
//             FROM pagos pg
//             -- Usamos LEFT JOIN para no perder pagos que quizás no tengan un préstamo asociado (como gastos directos)
//             LEFT JOIN prestamos pr ON pg.idprestamos = pr.idprestamos
//             WHERE 
//                 (
//                     -- CASO '01': Todos los ingresos globales
//                     $1 = '01' AND pg.numamortizacion = 'IGR'
//                 )
//                 OR 
//                 (
//                     -- CASO '02': Todos los registros cuyo préstamo sea de tipo 'AYUDA'
//                     -- Aquí usamos la columna de la tabla prestamos (pr.tipo)
//                     $1 = '02' AND pr.tipo LIKE 'AYUDA%'
//                 )
//                 OR 
//                 (
//                     -- CASO GENERAL: Filtrar por el ID del socio solicitado
//                     -- Excluimos los casos especiales para evitar duplicados si el socio fuera el 01 o 02
//                     $1 NOT IN ('01', '02') AND pg.idsocio = $1
//                 )
//             ORDER BY pg.fechaamortizacion ASC
//         `;

//         const result = await pool.query(query, [idsocio]);
//         res.json(result.rows);

//     } catch (err) {
//         console.error("Error en el servidor:", err.message);
//         res.status(500).json({ error: "Error al obtener pagos con relación a préstamos" });
//     }
// });

app.get('/api/pagos/socio/:idsocio', async (req, res) => {
    const { idsocio } = req.params;
    try {
        const query = `
            -- 1. CONSULTA DE PAGOS REALES
            SELECT 
                pg.idpagos, pg.idprestamos, pg.idsocio, pg.amortizacion, 
                pg.fechaamortizacion, pg.numamortizacion, pg.interes, 
                pg.fechainteres, pg.numinteres, pg.comentario, 
                pr.tipo,
                -- ASIGNACIÓN MANUAL DEL NOMBRE SEGÚN EL ID
                CASE 
                    WHEN $1 = '01' THEN 'SOCIOS INGRESOS'
                    WHEN $1 = '02' THEN 'SOCIOS GASTOS'
                    ELSE s.nombresocio 
                END AS nombresocio
            FROM pagos pg
            INNER JOIN socios s ON pg.idsocio = s.idsocio
            LEFT JOIN prestamos pr ON pg.idprestamos = pr.idprestamos
            WHERE 
                (pg.idsocio = $1 AND $1 NOT IN ('01', '02'))
                OR
                ($1 = '01' AND pg.numamortizacion = 'IGR')
                OR
                ($1 = '02' AND pg.idsocio = '02')

            UNION ALL

            -- 2. CONSULTA DE AYUDAS (SOLO PARA SOCIO 02)
            SELECT 
                NULL AS idpagos, pr.idprestamos, pr.idsocio, 
                pr.montoprestado AS amortizacion, pr.fechaprestamo AS fechaamortizacion, 
                'AYUDA' AS numamortizacion, 0 AS interes, NULL AS fechainteres, 
                'AYUDA' AS numinteres, 'REGISTRO DE AYUDA SOCIAL' AS comentario, 
                pr.tipo,
                'SOCIOS GASTOS' AS nombresocio -- ASIGNACIÓN MANUAL DIRECTA
            FROM prestamos pr
            WHERE $1 = '02' 
              AND pr.tipo LIKE 'AYUDA%' 
              AND pr.montoprestado > 0

            ORDER BY fechaamortizacion ASC
        `;

        const result = await pool.query(query, [idsocio]);
        res.json(result.rows);

    } catch (err) {
        console.error("Error detalle:", err.message);
        res.status(500).json({ error: "Error al procesar la consulta" });
    }
});

//////////////////////////////// ACTUALIZAR UN PAGO EXISTENTE //////////////////////////
app.put('/api/pagos/:id', async (req, res) => {
    const { id } = req.params;
    const { idsocio, nombresocio, idprestamos, amortizacion, fechaamortizacion, numamortizacion, interes, fechainteres, numinteres, comentario } = req.body;

    try {
        const query = `
            UPDATE pagos 
            SET idsocio = $1, nombresocio = $2, idprestamos = $3, amortizacion = $4, 
                fechaamortizacion = $5, numamortizacion = $6, interes = $7, 
                fechainteres = $8, numinteres = $9, comentario = $10
            WHERE idpagos = $11 RETURNING *`;

        const values = [idsocio, nombresocio, idprestamos, amortizacion, fechaamortizacion, numamortizacion, interes, fechainteres, numinteres, comentario, id];
        const result = await pool.query(query, values);
        await actualizarTodoElSocio(req.body.idsocio);
        res.json(result.rows[0]);
    } catch (err) {
        console.error(err);
        res.status(500).send('Error del servidor');
    }
});

///////////////////////////// ELIMINAR UN PAGO POR ID //////////////////////////
// app.delete('/api/pagos/:id', async (req, res) => {
//     const { id } = req.params;
//     try {
//         await pool.query('DELETE FROM pagos WHERE idpagos = $1', [id]);
//         await actualizarTodoElSocio(idsocio); // <--- RECALCULAR
//         res.json({ message: "Eliminado correctamente" });
//     } catch (err) {
//         res.status(500).send("Error al eliminar");
//     }
// });
app.delete('/api/pagos/:id', async (req, res) => {
    const { id } = req.params;
    try {
        // 1. Buscamos el idsocio antes de borrar el registro
        const resPago = await pool.query('SELECT idsocio FROM pagos WHERE idpagos = $1', [id]);

        if (resPago.rows.length === 0) {
            return res.status(404).json({ error: "Pago no encontrado" });
        }

        const idsocio = resPago.rows[0].idsocio;

        // 2. Eliminamos el pago
        await pool.query('DELETE FROM pagos WHERE idpagos = $1', [id]);

        // 3. Ahora sí, recalculamos con el idsocio que guardamos
        await actualizarTodoElSocio(req.body.idsocio);

        res.json({ message: "Pago eliminado y saldos del socio actualizados" });
    } catch (err) {
        console.error(err.message);
        res.status(500).send("Error al eliminar el pago");
    }
});


//**********************************SERVICIOS PARA CAJA***************************************

//////////////TRAER TODA LA LISTA DE CAJA //////////////////////////
app.get('/api/caja', async (req, res) => {
    try {
        const result = await pool.query('SELECT * FROM caja ORDER BY fechacaja DESC');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err.message);
        res.status(500).json({ error: 'Error al obtener los registros de caja' });
    }
});

//////////////////////GENERAR EL SIGUIENTE ID PARA NUEVO APORTE////////////////////////
app.get('/api/caja/siguiente-id', async (req, res) => {
    try {
        // Buscamos el valor máximo actual
        const query = 'SELECT MAX(idcaja) FROM caja';
        const result = await pool.query(query);

        // Si no hay aportes, empezamos en 1. Si hay, sumamos 1 al máximo.
        const ultimoId = result.rows[0].max || 0;
        const siguienteId = ultimoId + 1;

        // Devolvemos el número
        res.json({ siguienteId });
    } catch (err) {
        console.error(err);
        res.status(500).json({ error: 'Error al calcular el siguiente ID' });
    }
});

///////////////////////TRAER UN APORTE POR ID //////////////////////////
app.get('/api/caja/:id', async (req, res) => {
    const { id } = req.params;
    try {
        const result = await pool.query('SELECT * FROM caja WHERE idcaja = $1', [id]);
        if (result.rows.length > 0) {
            res.json(result.rows[0]); // Devuelve el primer registro encontrado
        } else {
            res.status(404).json({ message: "Registro de caja no encontrado" });
        }
    } catch (err) {
        res.status(500).send("Error en el servidor");
    }
});

////////////////////////// GUARDAR NUEVA CAJA //////////////////////////
// app.post('/api/caja', async (req, res) => {
//     const { idcaja, efectivo, fechacaja, comentario } = req.body;

//     try {
//         const query = `
//             INSERT INTO caja (idcaja, efectivo, fechacaja, comentario)
//             VALUES ($1, $2, $3, $4)
//             RETURNING *`;

//         // Asegúrate de que idsocio llegue como string y no como [object Object]
//         const values = [idcaja, efectivo, fechacaja, comentario];
//         const result = await pool.query(query, values);
//         res.status(201).json(result.rows[0]);
//     } catch (err) {
//         console.error("Error SQL:", err.detail || err.message);
//         res.status(500).json({ error: 'Error al guardar: verifique si el ID ya existe' });
//     }
// });
app.post('/api/caja', async (req, res) => {
    const { idcaja, efectivo, fechacaja, comentario } = req.body;

    // 1. Calcular el rango del período basado en fechacaja
    const fechaActual = new Date(fechacaja);
    let inicioPeriodo = new Date(fechaActual);
    let finPeriodo = new Date(fechaActual);

    if (fechaActual.getDate() >= 15) {
        // Ejemplo: 16 de Enero -> Rango: 15/Ene al 14/Feb
        inicioPeriodo.setDate(15);
        finPeriodo.setMonth(finPeriodo.getMonth() + 1);
        finPeriodo.setDate(14);
    } else {
        // Ejemplo: 10 de Enero -> Rango: 15/Dic al 14/Ene
        inicioPeriodo.setMonth(inicioPeriodo.getMonth() - 1);
        inicioPeriodo.setDate(15);
        finPeriodo.setDate(14);
    }

    // Normalizar horas para evitar errores de comparación
    inicioPeriodo.setHours(0, 0, 0, 0);
    finPeriodo.setHours(23, 59, 59, 999);

    try {
        // 2. Verificar si ya existe un registro en ese rango
        const checkQuery = `
            SELECT idcaja FROM caja 
            WHERE fechacaja BETWEEN $1 AND $2 
            LIMIT 1`;

        const checkResult = await pool.query(checkQuery, [inicioPeriodo, finPeriodo]);

        if (checkResult.rows.length > 0) {
            return res.status(400).json({
                error: `Ya existe un registro para el periodo ${inicioPeriodo.toLocaleDateString()} al ${finPeriodo.toLocaleDateString()}`
            });
        }

        // 3. Si no existe, procedemos al INSERT
        const insertQuery = `
            INSERT INTO caja (idcaja, efectivo, fechacaja, comentario)
            VALUES ($1, $2, $3, $4)
            RETURNING *`;

        const values = [idcaja, efectivo, fechacaja, comentario];
        const result = await pool.query(insertQuery, values);

        res.status(201).json(result.rows[0]);

    } catch (err) {
        console.error("Error SQL:", err.detail || err.message);
        res.status(500).json({ error: 'Error interno del servidor' });
    }
});

////////////////////////// ACTUALIZAR CAJA EXISTENTE //////////////////////////
app.put('/api/caja/:id', async (req, res) => {
    const { id } = req.params;
    const { efectivo, fechacaja, comentario } = req.body;

    try {
        const query = `
            UPDATE caja 
            SET efectivo = $1, 
                fechacaja = $2, 
                comentario = $3
            WHERE idcaja = $4
            RETURNING *`;

        // AQUÍ ESTABA EL ERROR: Deben ser 6 valores exactamente.
        const values = [
            efectivo, // $1
            fechacaja,     // $2
            comentario,   // $3
            id            // $4
        ];

        const result = await pool.query(query, values);

        if (result.rows.length > 0) {
            res.json(result.rows[0]);
        } else {
            res.status(404).json({ message: "Caja no encontrada" });
        }
    } catch (err) {
        console.error("Error al actualizar:", err);
        res.status(500).json({ error: "Error interno del servidor" });
    }
});

/////////////////////////////// ELIMINAR UN CAJA POR ID //////////////////////////
app.delete('/api/caja/:id', async (req, res) => {
    const { id } = req.params;
    try {
        await pool.query('DELETE FROM caja WHERE idcaja = $1', [id]);
        res.json({ message: "Eliminado correctamente" });
    } catch (err) {
        res.status(500).send("Error al eliminar");
    }
});


//**********************************SERVICIOS PARA REPORTES***************************************
// async function actualizarTodoElSocio(idsocio) {
//     try {
//         // 1. Sumas de Aportes, Préstamos y Pagos (Asegúrate de usar montoprestado aquí)
//         const aporte = await pool.query('SELECT SUM(aportado) as total FROM aportes WHERE idsocio=$1', [idsocio]);
//         const prestamo = await pool.query('SELECT SUM(montoprestado) as m, SUM(interestotal) as i FROM prestamos WHERE idsocio=$1 AND idprestamos NOT LIKE $2', [idsocio, 'AYUDA%']);

//         // 2. Cálculo de pagos excluyendo ingresos/egresos
//         const pagos = await pool.query(`
//             SELECT 
//                 SUM(CASE WHEN idpagos NOT LIKE 'EGRESO%' AND idpagos NOT LIKE 'INGRESO%' AND numamortizacion != 'GST' THEN amortizacion ELSE 0 END) as cap_pag,
//                 SUM(CASE WHEN numamortizacion = 'GST' THEN amortizacion ELSE 0 END) as cap_anu,
//                 SUM(CASE WHEN idpagos NOT LIKE 'EGRESO%' AND idpagos NOT LIKE 'INGRESO%' AND numinteres != 'GST' THEN interes ELSE 0 END) as int_pag,
//                 SUM(CASE WHEN numinteres = 'GST' THEN interes ELSE 0 END) as int_anu
//             FROM pagos WHERE idsocio = $1`, [idsocio]);

//         // 3. UPDATE físico en la tabla SOCIOS
//         const queryUpdate = `
//             UPDATE socios SET 
//                 aportado = $1, montoprestado = $2, montopagado = $3,
//                 montopendiente = (CAST($2 AS NUMERIC) - (CAST($3 AS NUMERIC) + CAST($4 AS NUMERIC))),
//                 interesprestado = $5, interespagado = $6, interesanulado = $7,
//                 interespendiente = (CAST($5 AS NUMERIC) - (CAST($6 AS NUMERIC) + CAST($7 AS NUMERIC)))
//             WHERE idsocio = $8`;

//         await pool.query(queryUpdate, [
//             Number(aporte.rows[0].total || 0),
//             Number(prestamo.rows[0].m || 0),
//             Number(pagos.rows[0].cap_pag || 0),
//             Number(pagos.rows[0].cap_anu || 0),
//             Number(prestamo.rows[0].i || 0),
//             Number(pagos.rows[0].int_pag || 0),
//             Number(pagos.rows[0].int_anu || 0),
//             idsocio
//         ]);

//         console.log(`Datos físicos actualizados para el socio: ${idsocio}`);
//     } catch (error) {
//         console.error("Error en actualizarTodoElSocio:", error.message);
//     }
// }
async function actualizarTodoElSocio(idsocio) {
    try {
        // 1. Sumas de Aportes
        const aporte = await pool.query('SELECT SUM(aportado) as total FROM aportes WHERE idsocio=$1', [idsocio]);

        // 2. Sumas de Préstamos (Filtrando por la columna 'tipo')
        // Usamos "=" porque idsocio es numérico y comparamos 'tipo' para excluir AYUDA
        const prestamo = await pool.query(`
            SELECT SUM(montoprestado) as m, SUM(interestotal) as i 
            FROM prestamos 
            WHERE idsocio = $1 AND tipo = 'PRESTAMO'`, [idsocio]);

        // 3. Cálculo de pagos usando la columna 'tipo'
        // Eliminamos los "LIKE" sobre los IDs numéricos
        const pagos = await pool.query(`
            SELECT 
                SUM(CASE WHEN tipo = 'PAGO' AND numamortizacion != 'GST' THEN amortizacion ELSE 0 END) as cap_pag,
                SUM(CASE WHEN numamortizacion = 'GST' THEN amortizacion ELSE 0 END) as cap_anu,
                SUM(CASE WHEN tipo = 'PAGO' AND numinteres != 'GST' THEN interes ELSE 0 END) as int_pag,
                SUM(CASE WHEN numinteres = 'GST' THEN interes ELSE 0 END) as int_anu
            FROM pagos WHERE idsocio = $1`, [idsocio]);

        // 4. UPDATE físico en la tabla SOCIOS
        const queryUpdate = `
            UPDATE socios SET 
                aportado = $1, montoprestado = $2, montopagado = $3,
                montopendiente = (CAST($2 AS NUMERIC) - (CAST($3 AS NUMERIC) + CAST($4 AS NUMERIC))),
                interesprestado = $5, interespagado = $6, interesanulado = $7,
                interespendiente = (CAST($5 AS NUMERIC) - (CAST($6 AS NUMERIC) + CAST($7 AS NUMERIC)))
            WHERE idsocio = $8`;

        await pool.query(queryUpdate, [
            Number(aporte.rows[0].total || 0),
            Number(prestamo.rows[0].m || 0),
            Number(pagos.rows[0].cap_pag || 0),
            Number(pagos.rows[0].cap_anu || 0),
            Number(prestamo.rows[0].i || 0),
            Number(pagos.rows[0].int_pag || 0),
            Number(pagos.rows[0].int_anu || 0),
            idsocio
        ]);

        console.log(`Datos físicos actualizados para el socio: ${idsocio}`);
    } catch (error) {
        // Esto evitará el error "integer !~~ unknown"
        console.error("Error en actualizarTodoElSocio:", error.message);
    }
}

//////////////////////////PETICIONES PARA INFORMES /////////////////////
// PETICIONES PARA INFORMES //
app.get('/api/reportes/aportes', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(aportado), 0) as total 
            FROM aportes 
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroAportado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte aportes:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/aportestotales', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(aportado), 0) as total 
            FROM aportes 
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            totalAportados: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte aportes:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/aportesmensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(aportado), 0) as total 
            FROM aportes 
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroAportado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte aportes:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/prestamos', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroEnPrestamo: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/prestamosmensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPrestado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/ayudas', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroAyuda: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/prestamos/mensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(montoprestado), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPrestadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte prestamos:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interesprestamos', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interestotal), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesEnPrestamo: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes prestamo:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interesprestamosmensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interestotal), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPrestado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes prestamo:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interesprestamos/mensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interestotal), 0) as total 
            FROM prestamos 
            WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
            AND tipo NOT LIKE 'AYUDA%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPrestadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes prestamo mensual:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/dineropagado', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo NOT LIKE 'INGRESO%' AND idsocio!= '02'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPaga: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/dineropagadomensualefectivo', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo NOT LIKE 'INGRESO%' AND idsocio!= '02' AND comentario !~* 'RE+PRESTAMO'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPagadoEfectivo: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/dineropagadomensualreprestamo', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(
                SUM(
                    CASE 
                        WHEN tipo NOT LIKE 'INGRESO%' AND idsocio != '02' AND (comentario LIKE '%REPRESTAMO%' OR comentario LIKE '%REEPRESTAMO%')
                        THEN (COALESCE(amortizacion, 0))

                        WHEN tipo LIKE 'EGRESO%' AND idsocio= '02' 
                        THEN -(COALESCE(amortizacion, 0) + COALESCE(interes, 0)) -- El signo (-) resta este grupo

                        ELSE 0 
                    END
                ), 0) as total_neto
            FROM pagos
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date;
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPagadoReprestamo: parseFloat(result.rows[0].total_neto)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/dineropagado/mensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo NOT LIKE 'EGRESO%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            dineroPagadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte dinero pagado mensual:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interespagado', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interes), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo NOT LIKE 'INGRESO%' AND idsocio!= '02'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPagado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interespagadomensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interes), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo NOT LIKE 'EGRESO%'
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPagado: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes pagado:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interespagado/mensual', async (req, res) => {
    const { inicio, fin } = req.query;

    // Validación de fechas
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({
            message: 'La fecha de inicio no puede ser mayor a la de fin'
        });
    }

    try {
        // Consulta SQL usando COALESCE para evitar nulos
        const query = `
            SELECT COALESCE(SUM(interes), 0) as total --+ COALESCE(SUM(amortizacion), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo NOT LIKE 'EGRESO%'; 
        `;

        const result = await pool.query(query, [inicio, fin]);

        res.json({
            interesPagadoMensual: parseFloat(result.rows[0].total)
        });
    } catch (error) {
        console.error("Error en reporte interes pagado mensual:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/ingresosvarios', async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(COALESCE(amortizacion, 0) + COALESCE(interes, 0)), 0) as total 
            FROM pagos 
            WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
            AND tipo LIKE 'INGRESO%' AND idsocio != '02'
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[INGRESOS VARIOS] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total}`);

        res.json({ ingresosVarios: parseFloat(result.rows[0].total) });
    } catch (error) {
        console.error("Error en reporte ingresos varios:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/gastosvarios', async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(subconsulta.total), 0) as total_general
            FROM (
                -- Total de Egresos (Gastos/Multas)
                SELECT SUM(COALESCE(amortizacion, 0) + COALESCE(interes, 0)) as total 
                FROM pagos 
                WHERE fechaamortizacion::date >= $1::date AND fechaamortizacion::date <= $2::date 
                AND tipo LIKE 'EGRESO%' AND idsocio = '02'

                UNION ALL

                -- Total de Préstamos Entregados (Ayudas)
                SELECT SUM(montoprestado) as total 
                FROM prestamos 
                WHERE fechaprestamo::date >= $1::date AND fechaprestamo::date <= $2::date 
                AND tipo LIKE 'AYUDA%'
            ) as subconsulta;
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[GASTOS VARIOS] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total_general}`);

        res.json({ egresosSocios: parseFloat(result.rows[0].total_general) });
    } catch (error) {
        console.error("Error en reporte gastos varios:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/interesanulado', async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(COALESCE(interes, 0)), 0) as total 
            FROM pagos 
            WHERE fechainteres::date >= $1::date AND fechainteres::date <= $2::date 
            AND tipo LIKE 'EGRESO%' AND idsocio != '02';
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[INTERÉS ANULADO] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total}`);

        res.json({ egresoInteresAnulado: parseFloat(result.rows[0].total) });
    } catch (error) {
        console.error("Error en reporte intereses anulado:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/aporteindividual', async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(
                SUM(COALESCE(aportado, 0)) / NULLIF((SELECT COUNT(*) FROM socios WHERE idsocio NOT IN ('01', '02')), 0), 
                0
            ) as total_promedio
            FROM aportes
            WHERE fechaaporte::date >= $1::date AND fechaaporte::date <= $2::date
            AND idsocio NOT IN ('01', '02')
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[APORTE INDIVIDUAL] Rango: ${inicio} a ${fin} | Promedio: ${result.rows[0].total_promedio}`);

        // IMPORTANTE: Aquí cambié el nombre a 'total_promedio' para que coincida con tu SQL
        res.json({ aporteIndividual: parseFloat(result.rows[0].total_promedio) });
    } catch (error) {
        console.error("Error en reporte aporte individual:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/dinerocaja', async (req, res) => {
    const { inicio, fin } = req.query;
    if (new Date(inicio) > new Date(fin)) {
        return res.status(400).json({ message: 'La fecha de inicio no puede ser mayor a la de fin' });
    }

    try {
        const query = `
            SELECT COALESCE(SUM(COALESCE(efectivo, 0)), 0) as total 
            FROM caja 
            WHERE fechacaja::date >= $1::date AND fechacaja::date <= $2::date;
        `;
        const result = await pool.query(query, [inicio, fin]);

        // LOG DE DEPURACIÓN
        console.log(`[DINERO CAJA] Rango: ${inicio} a ${fin} | Total: ${result.rows[0].total}`);

        res.json({ dineroEnCaja: parseFloat(result.rows[0].total) });
    } catch (error) {
        console.error("Error en reporte dinero en caja:", error.message);
        res.status(500).json({ error: error.message });
    }
});

app.get('/api/reportes/caja-anterior', async (req, res) => {
    const { inicio, fin } = req.query;

    try {
        // 1. Convertimos las fechas recibidas a objetos Date
        let fechaInicioPrevia = new Date(inicio);
        let fechaFinPrevia = new Date(fin);

        // 2. Restamos 1 mes a cada fecha para obtener el periodo anterior
        fechaInicioPrevia.setMonth(fechaInicioPrevia.getMonth() - 1);
        fechaFinPrevia.setMonth(fechaFinPrevia.getMonth() - 1);

        // 3. Consulta SQL sumando la caja en ese nuevo rango
        const query = `
            SELECT COALESCE(SUM(efectivo), 0) as total 
            FROM caja 
            WHERE fechacaja::date >= $1::date 
              AND fechacaja::date <= $2::date
        `;

        const result = await pool.query(query, [
            fechaInicioPrevia.toISOString().split('T')[0],
            fechaFinPrevia.toISOString().split('T')[0]
        ]);

        const totalPrevio = parseFloat(result.rows[0].total);

        console.log(`[CAJA ANTERIOR] Rango: ${fechaInicioPrevia.toLocaleDateString()} a ${fechaFinPrevia.toLocaleDateString()} | Total: ${totalPrevio}`);

        res.json({ dineroCajaAnterior: totalPrevio });

    } catch (error) {
        console.error("Error en caja anterior:", error.message);
        res.status(500).json({ error: error.message });
    }
});

// Iniciar servidor
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`Servidor de la Cooperativa corriendo en el puerto ${PORT}`);
});